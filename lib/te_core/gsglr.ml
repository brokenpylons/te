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

  let push u v n g =
    assert (not (G.vertex_mem u g));
    g
    |> G.add u ()
    |> G.connect u v n

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
  type states := T.State.t
  type statess := T.States.t

  type symbol = Null | Code of T.Code.t | Var of T.Var.t

  val start: t -> statess 
  val actions: t -> statess -> symbol -> actions 
  val goto: t -> statess -> symbol -> statess option
  val back: t -> states * states -> states -> (states * states) option

  val shift: actions -> bool
  val matches: actions -> T.Labeled_vars.t
  val predictions: actions -> T.Labeled_vars.t
  val null: actions -> T.Reductions.t
  val reduce: actions -> T.Reductions.t
end

module Subclasses = Multimap.Make2(Balanced_binary_tree.Map.Size(Int))(T.Vertices)
module Segments = Multimap.Make2(T.Vertex_to)(T.Vertices)
module Orders = Multimap.Make2(T.Var_to)(T.Vertices)
module Orders' = Multimap.Make2(T.Vertex_to)(Orders.Set)

module Make(Tables: TABLES) = struct
  class driver (t: Tables.t) =
    let bottom = T.Vertex.make (Tables.start t) 0 in object(self)
    val mutable stack = Gss.singleton bottom
    val mutable forest = Forest.empty
    val mutable reduce = Segments.singleton bottom bottom
    val mutable read0 = Segments.empty
    val mutable read1 = Segments.empty
    val mutable subclasses = Subclasses.singleton 0 bottom
    val mutable orders = Orders'.empty

    initializer
      self#expand bottom

    method private pp_symbol ppf = function
      | Tables.Null -> Fmt.string ppf "NULL"
      | Tables.Code x -> T.Code.pp ppf x
      | Tables.Var x -> T.Var.pp ppf x

    method private successors v ns paths d =
      if Int.equal d 0
      then Paths.singleton (v, ns)
      else Paths.fold (fun (v', ns') -> Paths.union @@ self#successors v' ns' (Gss.adjacent v' ns' stack) (pred d)) Paths.empty paths

    method private scan_back v ns paths p =
      Fmt.pr "SB %a %a@," T.Vertex.pp v (Fmt.parens Paths.pp) paths;
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
          List.map (fun x -> T.Node.make x pos pos) xs)
        xss

    method private find_paths v l (strategy: T.Reduction.Strategy.t)  =
      let init = T.Vertices.fold (fun v' -> Paths.add (v', [!!(Gss.node v v' stack)])) Paths.empty l in
      Paths.to_seq @@ match strategy with
      | Fixed d -> self#successors v [] init d
      | Scan p -> self#scan_back v [] init p
      | Null -> Paths.singleton (v, [])

    method private actor ~null v l xs = 
      Fmt.pr "actor %b %a %a %a@," null T.Vertex.pp v T.Vertices.pp l (Fmt.list self#pp_symbol) xs;
      List.iter (fun x ->
          let a = Tables.actions t (T.Vertex.states v) x in
          (if Tables.shift a then
             match x with
             | Var x ->
               Fmt.pr "ORDER %a %a@," T.Vertex.pp v T.Var.pp x;
               orders <- Orders'.add_multiple v (Orders.singleton x v) orders
             | _ -> ());
          if not (T.Labeled_vars.is_empty @@ Tables.matches a) then begin
            self#prediction l (List.map (fun output -> Tables.Var (T.Labeled_var.var output)) (T.Labeled_vars.to_list @@ Tables.predictions a));
          end;
          if not null then begin
            T.Reductions.iter (fun r ->
                self#reduce v l r xs) (Tables.reduce a)
          end;
          T.Reductions.iter (fun r ->
              self#reduce v T.Vertices.empty r xs)
            (Tables.null a);
          Seq.iter (fun output ->
              self#shift v l output)
            (T.Labeled_vars.to_seq @@ Tables.matches a))
        xs

    (*method private null v r xs =
      (match Tables.goto t (T.Vertex.states v) (Var (T.Labeled_var.var r.output)) with
       | Some s ->
         let pos = T.Vertex.position v in
         let u = T.Vertex.make s pos in
         let n = T.Node.make (T.Labeled_var.var r.output) pos pos in
         Fmt.pr "NULL %a %a -> %a %a@," T.Vertex.pp v T.Vertex.pp u T.Labeled_var.pp r.output (Fmt.list self#pp_symbol) xs;
         if not (Gss.contains u stack) then begin
           stack <- Gss.add u stack;
           subclasses <- Subclasses.add pos u subclasses;
         end;
         if not (Gss.contains_edge u v stack) then begin
           stack <- Gss.connect u v (Some n) stack;
           forest <- Forest.add n forest;
           self#actor ~null:true u (T.Vertices.singleton v) xs;
           orders <- Orders'.add_multiple v (Orders'.find_multiple_or ~default:Orders.empty u orders) orders
         end;
         List.iter (fun ns' ->
             forest <- Forest.pack n (T.Labeled_var.label r.output) (ns') forest)
           (self#enumerate pos r.reminder)
       | None -> ())*)

    method private shift v l output =
      Seq.iter (fun w ->
          Seq.iter (fun v' ->
              (match Tables.goto t (T.Vertex.states v') (Var (T.Labeled_var.var output)) with
               | Some s ->
                 let pos = T.Vertex.position v in
                 let u = T.Vertex.make s pos in
                 let n = T.Node.make (T.Labeled_var.var output) (T.Vertex.position v') pos in
                 Fmt.pr "SHIFT %a %a %a %a -> %a@," T.Vertex.pp v T.Vertex.pp w T.Vertex.pp v' T.Vertex.pp u T.Labeled_var.pp output;
                 if not (Gss.contains u stack) then begin
                   stack <- Gss.add u stack;
                   subclasses <- Subclasses.add pos u subclasses;
                   self#expand u
                 end;
                 if not (Gss.contains_edge u v' stack) then begin
                   stack <- Gss.connect u v' (Some n) stack;
                   reduce <- Segments.add u v' reduce;
                   forest <- Forest.add n forest;
                   forest <- Forest.pack n (T.Vars.singleton @@ T.Labeled_var.label output) [] forest
                 end
               | None -> ()))
            (Orders'.find_multiple_or ~default:Orders.empty w orders
             |> Orders.find_multiple_or ~default:T.Vertices.empty (T.Labeled_var.var output)
             |> T.Vertices.to_seq))
        (T.Vertices.to_seq l)

    method private prediction l xs =
      Seq.iter (fun w ->
          if not (Orders'.domain_mem w orders) then begin
            Fmt.pr "PREDICTION %a %a@," T.Vertex.pp w (Fmt.list self#pp_symbol) xs;
            self#actor ~null:false w (Segments.find_multiple w reduce) xs
          end)
        (T.Vertices.to_seq l)

    method private reduce v l r xs =
      Seq.iter (fun (w, ns) ->
          (match Tables.goto t (T.Vertex.states w) (Var (T.Labeled_var.var r.output)) with
           | Some s ->
             let pos = T.Vertex.position v in
             let u = T.Vertex.make s pos in
             let n = T.Node.make (T.Labeled_var.var r.output) (T.Vertex.position w) pos in
             Fmt.pr "REDUCE %a %a %a -> %a@," T.Vertex.pp v T.Vertex.pp w T.Vertex.pp u T.Labeled_var.pp r.output;
             if not (Gss.contains u stack) then begin
               stack <- Gss.add u stack;
               subclasses <- Subclasses.add pos u subclasses
             end;
             if not (Gss.contains_edge u w stack) then begin
               stack <- Gss.connect u w (Some n) stack;
               forest <- Forest.add n forest;
               self#actor ~null:(match r.strategy with Null -> true | _ -> false) u (T.Vertices.singleton w) xs;
               orders <- Orders'.add_multiple v (Orders'.find_multiple_or ~default:Orders.empty u orders) orders
             end;
             List.iter (fun ns' ->
                 forest <- Forest.pack n (T.Vars.singleton @@ T.Labeled_var.label r.output) (ns @ ns') forest)
               (self#enumerate pos r.reminder)
           | None -> ()))
        (self#find_paths v l r.strategy)

    method private expand v =
      match Tables.goto t (T.Vertex.states v) Null with
      | Some s ->
        (let pos = T.Vertex.position v in
         let u = T.Vertex.make s pos in
         Fmt.pr "EXPAND %a %a@," T.Vertex.pp v T.Vertex.pp u;
         if not (Gss.contains u stack) then begin
           stack <- Gss.add u stack;
           subclasses <- Subclasses.add pos u subclasses;
         end;
         if not (Gss.contains_edge u v stack) then begin
           stack <- Gss.connect u v None stack;
           read1 <- Segments.add u v read1
         end)
      | None -> ()

    method read x = begin
      Fmt.pr "@,CODE %a@," self#pp_symbol x;
      Fmt.pr "R %a@," (T.Vertex_to.pp (Fmt.parens T.Vertices.pp)) read1;
      Seq.iter (fun (w, l) ->
          self#actor ~null:false w l [x])
        (Segments.to_seq_multiple read1);
      Fmt.pr "R1 %a@," (T.Vertex_to.pp (Fmt.parens T.Vertices.pp)) read1;
      read0 <- read1;
      read1 <- Segments.empty;
      Fmt.pr "AFTER %a@," (T.Vertex_to.pp (Fmt.parens T.Vertices.pp)) read0;
      Seq.iter (fun (w, l) ->
          Seq.iter (fun v ->
              match Tables.goto t (T.Vertex.states w) x with
              | Some s ->
                let pos = succ @@ T.Vertex.position w in
                let u = T.Vertex.make s pos in
                Fmt.pr "READ %a %a %a -> %a@," T.Vertex.pp w T.Vertex.pp v T.Vertex.pp u self#pp_symbol x;
                if not (Gss.contains u stack) then begin
                  stack <- Gss.add u stack;
                  subclasses <- Subclasses.add pos u subclasses;
                end;
                if not (Gss.contains_edge u v stack) then begin
                  stack <- Gss.connect u v None stack;
                  read1 <- Segments.add u v read1
                end
              | None -> ())
            (T.Vertices.to_seq l))
        (Segments.to_seq_multiple read0)
    end

    method forest =
      forest

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

