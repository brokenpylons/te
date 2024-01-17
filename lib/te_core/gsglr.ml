open Te_bot
open! Prelude
module T = Types

module Path = struct
  type t = T.Vertex.t * T.Node.t list
  [@@deriving ord, show]
end
module Paths = struct
  include Balanced_binary_tree.Set.Size(Path)
  let pp = pp Path.pp
end

module Gss = struct
  module G = T.Vertex_graph
  type t = (unit, T.Node.t option) G.t
  let singleton v = G.singleton v ()

  let add u g =
    assert (not (G.vertex_mem u g));
    g
    |> G.add u ()

  let connect u v n g =
    G.connect u v n g

  let contains u g =
    G.vertex_mem u g

  let contains_edge u v g =
    G.edge_mem u v g

  let node u v g =
    G.edge_label u v g

  let adjacent v ns g =
    G.adjacent v g
    |> Seq.map (fun (w, n) -> (w, (!!n :: ns)))
    |> Paths.of_seq
end

module Forest = T.Node_packed_forest

module type S = sig
  type t
  val empty: t
  val feed: T.Code.t -> t -> t
end

module type TABLES = sig
  type t
  type actions
  type state := T.State.t
  type states := T.States.t
  type statess := T.Statess.t

  type symbol := T.Symbol.t

  val start: t -> states
  val actions: t -> states -> symbol -> actions 
  val goto: t -> states -> symbol -> statess
  val back: t -> state * state -> state -> (state * state) option

  val actions_union: actions -> actions -> actions (* XXX: Replace with a struct *)
  val actions_empty: actions

  val shift: actions -> bool
  val shift_code: actions -> bool
  val orders: actions -> T.Vars.t
  val matches: actions -> T.Labeled_vars.t
  val predictions: actions -> T.Labeled_vars.t
  val null: actions -> T.Reductions.t
  val reduce: actions -> T.Reductions.t
end

module Subclasses = Multimap.Make2(Balanced_binary_tree.Map.Size(Int))(T.Vertices)
module Segments = Multimap.Make2(T.Vertex_to)(T.Vertices)
module Orders = Multimap.Make2(T.Var_to)(T.Vertices)
module Orders' = Multimap.Make2(T.Vertex_to)(Orders.Set)

(*
Order of operation is inour case a mix between breadth first and depth first search.
The synchronization point are the characters, the orther otherwise doesn't matter.
That way we need less explict datastructures, the data is held on the stack.
*)

module Make(Tables: TABLES) = struct

  class driver (t: Tables.t) =
    let bottom = T.Vertex.make (Tables.start t) 0 in object(self)
    val mutable stack = Gss.singleton bottom
    val mutable forest = Forest.empty
    val mutable reduce = Segments.singleton bottom bottom
    val mutable read0 = Segments.empty
    val mutable read1 = Segments.empty
    val mutable shift0 = Segments.empty
    val mutable shift1 = Segments.singleton bottom bottom
    val mutable subclasses = Subclasses.singleton 0 bottom
    val mutable starters = Subclasses.empty
    val mutable orders = Orders'.empty
    val mutable trace = []

    initializer
      self#expand bottom

    method private log action =
      trace <- action :: trace

    method private successors v ns paths d =
      if Int.equal d 0
      then Paths.singleton (v, ns)
      else Paths.fold (fun (v', ns') -> Paths.union @@ self#successors v' ns' (Gss.adjacent v' ns' stack) (pred d)) Paths.empty paths

    method private scan_back v ns paths p =
      (*Fmt.pr "SB %a %a@," T.Vertex.pp v (Fmt.parens Paths.pp) paths;*)
      if Paths.is_empty paths
      then Paths.singleton (v, ns)
      else Paths.fold (fun (v', ns') -> 
          Paths.union @@
          T.States.fold (fun s ->
              Paths.union @@
              match Tables.back t p s with
              | Some p' -> self#scan_back v' ns' (Gss.adjacent v' ns' stack) p'
              | None -> Paths.singleton (v, ns))
            Paths.empty (T.Vertex.states v'))
          Paths.empty paths

    method private enumerate pos xss =
      List.map (fun xs ->
          List.map (fun x -> T.Node.make (Var x) pos pos) xs)
        xss

    method private find_paths v l (strategy: T.Reduction.Strategy.t)  =
      let init = T.Vertices.fold (fun v' -> Paths.add (v', [!!(Gss.node v v' stack)])) Paths.empty l in
      Paths.to_seq @@ match strategy with
      | Fixed d -> self#successors v [] init d
      | Scan p -> self#scan_back v [] init p
      | Null -> Paths.singleton (v, [])

    method private actor ~null v l xs = 
      (*Fmt.pr "actor %b %a %a %a@," null T.Vertex.pp v T.Vertices.pp l (Fmt.list T.Symbol.pp) xs;*)
      let a = List.fold_left (fun acc x -> Tables.actions_union (Tables.actions t (T.Vertex.states v) x) acc) Tables.actions_empty xs in
      T.Vars.iter (fun x ->
          self#log (Trace.order v x);
          orders <- Orders'.add_multiple v (Orders.singleton x v) orders)
        (Tables.orders a);
      if not (T.Labeled_vars.is_empty @@ Tables.matches a) then begin
        self#prediction l (List.map (fun output -> T.Labeled_var.var output) (T.Labeled_vars.to_list @@ Tables.predictions a));
      end;
      if not null then begin
        T.Reductions.iter (fun r ->
            self#reduce v l r xs) (Tables.reduce a)
      end;
      T.Reductions.iter (fun r ->
          self#reduce v T.Vertices.empty r xs)
        (Tables.null a);
      Seq.iter (fun output ->
          Seq.iter (fun w ->
              let pos = T.Vertex.position v in
              self#shift pos w output)
            (T.Vertices.to_seq l))
        (T.Labeled_vars.to_seq @@ Tables.matches a);
      List.iter (fun x ->
          if (match x with T.Symbol.Eof -> true | T.Symbol.Code _ -> true | _ -> false) && Tables.shift_code @@ Tables.actions t (T.Vertex.states v) x then self#load v x)
        xs

    method private load v x =
      Tables.goto t (T.Vertex.states v) x |>
      T.Statess.iter (fun s ->
          let pos = succ @@ T.Vertex.position v in
          let u = T.Vertex.make s pos in
          let n = T.Node.make x (T.Vertex.position v) pos in
          self#log (Trace.load x v u);
          if not (Gss.contains u stack) then begin
            stack <- Gss.add u stack;
            subclasses <- Subclasses.add pos u subclasses;
            self#expand u
          end;
          if not (Gss.contains_edge u v stack) then begin
            stack <- Gss.connect u v (Some n) stack;
            reduce <- Segments.add u v reduce;
            shift1 <- Segments.add u v shift1;
            forest <- Forest.add n forest;
          end)

    method private shift pos w output =
      Seq.iter (fun v' ->
          Tables.goto t (T.Vertex.states v') (Var (T.Labeled_var.var output)) |>
          T.Statess.iter (fun s ->
              let u = T.Vertex.make s pos in
              let n = T.Node.make (Var (T.Labeled_var.var output)) (T.Vertex.position v') pos in
              self#log (Trace.shift output w v' u);
              if not (Gss.contains u stack) then begin
                stack <- Gss.add u stack;
                subclasses <- Subclasses.add pos u subclasses;
                self#expand u
              end;
              if not (Gss.contains_edge u v' stack) then begin
                stack <- Gss.connect u v' (Some n) stack;
                reduce <- Segments.add u v' reduce;
                shift1 <- Segments.add u v' shift1;
                forest <- Forest.add n forest;
              end;
              forest <- Forest.pack n (T.Vars.singleton @@ T.Labeled_var.label output) [] forest))
        (Orders'.find_multiple_or ~default:Orders.empty w orders
         |> Orders.find_multiple_or ~default:T.Vertices.empty (T.Labeled_var.var output)
         |> T.Vertices.to_seq)

    method private prediction l xs =
      Seq.iter (fun w ->
          if not (Orders'.domain_mem w orders) then begin
            self#log (Trace.predict w xs);
            self#actor ~null:false w (Segments.find_multiple w reduce) (List.map (fun x -> T.Symbol.Var x) xs)
          end)
        (T.Vertices.to_seq l)

    method private reduce v l r xs =
      Seq.iter (fun (w, ns) ->
          Tables.goto t (T.Vertex.states w) (Var (T.Labeled_var.var r.output)) |>
          T.Statess.iter (fun s ->
              let pos = T.Vertex.position v in
              let u = T.Vertex.make s pos in
              let n = T.Node.make (Var (T.Labeled_var.var r.output)) (T.Vertex.position w) pos in
              self#log (Trace.reduce r.output v w u);
              if not (Gss.contains u stack) then begin
                stack <- Gss.add u stack;
                subclasses <- Subclasses.add pos u subclasses
              end;
              if not (Gss.contains_edge u w stack) || not (Forest.mem n forest) then begin
                stack <- Gss.connect u w (Some n) stack;
                forest <- Forest.add n forest;
                self#actor ~null:(match r.strategy with Null -> true | _ -> false) u (T.Vertices.singleton w) xs;
                orders <- Orders'.add_multiple v (Orders'.find_multiple_or ~default:Orders.empty u orders) orders
              end;
              (*Fmt.pr "NODE %a@." T.Node.pp n;*)
              List.iter (fun ns' ->
                  forest <- Forest.pack n (T.Vars.singleton @@ T.Labeled_var.label r.output) (ns @ ns') forest)
                (self#enumerate pos r.reminder)))
        (self#find_paths v l r.strategy)

    method private expand v =
      Tables.goto t (T.Vertex.states v) Null |>
      T.Statess.iter (fun s ->
          (let pos = T.Vertex.position v in
           let u = T.Vertex.make s pos in
           self#log (Trace.expand v u);
           if not (Gss.contains u stack) then begin
             stack <- Gss.add u stack;
             subclasses <- Subclasses.add pos u subclasses;
           end;
           if not (Gss.contains_edge u v stack) then begin
             stack <- Gss.connect u v None stack;
             read1 <- Segments.add u v read1
           end))

    method read x = begin
      (*Fmt.pr "@,COMPLETE %a@," T.Symbol.pp x;*)
      (*Fmt.pr "R %a@," (T.Vertex_to.pp (Fmt.parens T.Vertices.pp)) read1;*)
      Seq.iter (fun (w, l) ->
          self#actor ~null:false w l [x])
        (Segments.to_seq_multiple read1);
      read0 <- read1;
      read1 <- Segments.empty;
      shift0 <- shift1;
      shift1 <- Segments.empty;
      (*Fmt.pr "AFTER %a@," (T.Vertex_to.pp (Fmt.parens T.Vertices.pp)) read0;*)
      Seq.iter (fun (w, l) ->
          if Tables.shift_code @@ Tables.actions t (T.Vertex.states w) x then
          self#actor ~null:false w l [x])
        (Segments.to_seq_multiple shift0);
      (*Fmt.pr "AFTER %a@," (T.Vertex_to.pp (Fmt.parens T.Vertices.pp)) read0;*)
      Seq.iter (fun (w, l) ->
          Seq.iter (fun v ->
              Tables.goto t (T.Vertex.states w) x |>
              T.Statess.iter (fun s ->
                  let pos = succ @@ T.Vertex.position w in
                  let u = T.Vertex.make s pos in
                  self#log (Trace.read x v w u );
                  if not (Gss.contains u stack) then begin
                    stack <- Gss.add u stack;
                    subclasses <- Subclasses.add pos u subclasses;
                  end;
                  if not (Gss.contains_edge u v stack) then begin
                    stack <- Gss.connect u v None stack;
                    read1 <- Segments.add u v read1
                  end))
            (T.Vertices.to_seq l))
        (Segments.to_seq_multiple read0);
    end

    method forest =
      forest

    method trace =
      List.rev trace

    method to_dot =
      let subgraphs = Subclasses.fold_multiple (fun position subclass subgraphs ->
          let nodes =
            subclass
            |> T.Vertices.to_list
            |> List.map (fun v ->
                Dot.(node (T.Vertex.to_id v) ~attrs:[
                    "xlabel" => String (Int.to_string @@ T.Vertex.position v);
                    "label" => String (Fmt.to_to_string T.States.pp (T.Vertex.states v));
                  ]))
          in
          let attrs = Dot.[
              Attr ("label" => HTML ("U<SUB>" ^ (Int.to_string position) ^ "</SUB>"))
            ]
          in
          Dot.Subgraph ("cluster_" ^ (Int.to_string position), attrs @ nodes) :: subgraphs)
          [] subclasses
      in
      let edges =
        Gss.G.labeled_edges stack
        |> List.of_seq
        |> List.map (fun (v1, v2, n) ->
            Dot.(edge Directed (T.Vertex.to_id v1) (T.Vertex.to_id v2) ~attrs:[
                "label" => String (Fmt.str "%a" (Fmt.option T.Node.pp) n)
              ]))
      in
      let attrs = Dot.[
          Attr ("rankdir" => Id "RL");
          Attr ("newrank" => Id "true")
        ]
      in
      Dot.((Digraph, "g", attrs @ subgraphs @ edges))
  end
end

