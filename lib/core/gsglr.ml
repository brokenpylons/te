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
  type t = (unit, T.Nodes.t) G.t
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
    |> Seq.flat_map (fun (w, n) ->
        T.Nodes.to_seq n
        |> Seq.map (fun n -> (w, (n :: ns))))
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
  type actions = T.Actions.t
  type state := T.State.t
  type states := T.States.t
  type state_pair := T.State_pair.t
  type state_pairs := T.State_pairs.t

  type symbol := T.Symbol.t

  val start: t -> state
  val actions: t -> state -> symbol -> actions
  val goto: t -> state -> symbol -> states
  val orders: t -> state -> T.Vars.t
  val back: t -> state_pair -> state -> symbol -> state_pairs
  val accept: t -> state -> bool

  val shift: actions -> bool
  val load: actions -> bool
  val matches: actions -> T.Labeled_vars.t
  val predictions: actions -> T.Vars.t
  val null: actions -> T.Reductions.t
  val reduce: actions -> T.Reductions.t
end

module Subclasses = Multimap.Make3(Balanced_binary_tree.Map.Size(Int))(T.Vertices)

module Segments = Multimap.Make3(T.Vertex_to)(T.Vertices)
module Orders = Multimap.Make3(T.Var_to)(T.Vertices)
module Orders' = Multimap.Make3(T.Vertex_to)(Orders.Set)

(*
Order of operation is in our case a mix between breadth first and depth first search.
The synchronization point are the characters, the orther otherwise doesn't matter.
That way we need less explict datastructures, the data is held on the stack.
*)

(*
The loads are not connected to scanner fragments.
The return links are needed for that (howerver just selectively, not for all symbols).
*)

(*
Lookahead needs to be recomputed for non-cannonical states otherwise it is slightly incorrect
*)

module Make(Tables: TABLES) = struct

  class driver (t: Tables.t) =
    let bottom = T.Vertex.make (Tables.start t) 0 in object(self)
      val mutable stack: Gss.t = Gss.singleton bottom
      val mutable forest = Forest.empty
      val mutable subclasses = Subclasses.singleton 0 bottom
      val mutable orders = Orders'.empty

      (* roots *)
      val mutable processed = T.Vertices.empty
      val mutable reduce = Segments.singleton bottom bottom
      val mutable read0 = Segments.empty
      val mutable read1 = Segments.empty
      val mutable shift0 = Segments.empty
      val mutable shift1 = Segments.singleton bottom bottom

      val mutable trace = []

      initializer
        self#order bottom;
        self#expand bottom

      method private log action =
        trace <- action :: trace

      method private successors v ns paths d =
        if Int.equal d 0
        then Paths.singleton (v, ns)
        else Paths.fold (fun (v', ns') ->
            Paths.union @@ self#successors v' ns' (Gss.adjacent v' ns' stack) (pred d))
            Paths.empty paths

      method private scan_back ~history v ns paths p =
        (*Fmt.pr "SB %a %a %a@," T.Vertex.pp v T.Vertices.pp history (Fmt.parens Paths.pp) paths;*)
        if T.Vertices.mem v history then
          assert false
        else
          let history = T.Vertices.add v history in
          if Paths.is_empty paths
          then Paths.singleton (v, ns)
          else Paths.fold (fun (v', ns') ->
              let n = match ns' with n :: _ -> n | [] -> assert false in
              let ps = Tables.back t p (T.Vertex.states v') (T.Node.symbol n) in
              Paths.union @@
              if T.State_pairs.is_empty ps then
                Paths.singleton (v, ns)
              else
                T.State_pairs.fold (fun p' ->
                    Paths.union @@
                    self#scan_back ~history v' ns' (Gss.adjacent v' ns' stack) p')
                  Paths.empty ps)
              Paths.empty paths

      method private enumerate pos (xss: T.Reduction.Reminder.t) =
        match xss with
        | Lists xss ->
          List.map (fun xs ->
              List.map (fun x -> T.Node.make (Var x) pos pos) xs)
            xss
        | Gen _ -> failwith "Not supported"
        | Complete -> [[]]

      method private find_paths ?filter v l (strategy: T.Reduction.Strategy.t)  =
        let init =
          match filter with
          | Some n ->
            Paths.singleton (T.Vertices.the l, [n])
          | None ->
            T.Vertices.fold (fun v' ps ->
                T.Nodes.fold (fun n ps ->
                    Paths.add (v', [n]) ps)
                  ps (Gss.node v v' stack))
              Paths.empty l
        in
        Paths.to_seq @@ match strategy with
        | Fixed d -> self#successors v [] init d
        | Scan p -> self#scan_back ~history:T.Vertices.empty v [] init p
        | Null -> Paths.singleton (v, [])

      method private actor ~null ?filter v l xs =
        (*Fmt.pr "actor %b %a %a %a@," null T.Vertex.pp v T.Vertices.pp l (Fmt.list T.Symbol.pp) xs;*)
        let a = List.fold_left (fun acc x ->
            T.Actions.union (Tables.actions t (T.Vertex.states v) x) acc)
            T.Actions.empty xs
        in
        if not (T.Labeled_vars.is_empty @@ Tables.matches a) then
          self#prediction v l (T.Vars.to_list @@ Tables.predictions a);
        if not null then
          T.Reductions.iter (fun r ->
              self#reduce ?filter v l r xs)
            (Tables.reduce a);
        T.Reductions.iter (fun r ->
            self#reduce v T.Vertices.empty r xs)
          (Tables.null a);
        Seq.iter (fun output ->
            Seq.iter (fun w ->
                let pos = T.Vertex.position v in
                self#shift pos v w output)
              (T.Vertices.to_seq l))
          (T.Labeled_vars.to_seq @@ Tables.matches a);
        List.iter (fun x ->
            if Tables.load @@ Tables.actions t (T.Vertex.states v) x
            then self#load v x)
          xs

      method private order v =
        T.Vars.iter (fun x ->
            self#log (Trace.order v x);
            orders <- Orders'.add_multiple v (Orders.singleton x v) orders)
          (Tables.orders t (T.Vertex.states v))

      method private load v x =
        Tables.goto t (T.Vertex.states v) x |>
        T.States.iter (fun s ->
            let pos = succ @@ T.Vertex.position v in
            let u = T.Vertex.make s pos in
            let n = T.Node.make x (T.Vertex.position v) pos in
            self#log (Trace.load x v u);
            if not (Gss.contains u stack) then begin
              stack <- Gss.add u stack;
              subclasses <- Subclasses.add pos u subclasses;
              self#order u;
              self#expand u
            end;
            if not (Gss.contains_edge u v stack) then begin
              stack <- Gss.connect u v (T.Nodes.singleton n) stack;
              reduce <- Segments.add u v reduce;
              shift1 <- Segments.add u v shift1;
              forest <- Forest.add n forest;
            end)

      method private shift pos v w output =
        Seq.iter (fun v' ->
            Tables.goto t (T.Vertex.states v') (Var (T.Labeled_var.var output)) |>
            T.States.iter (fun s ->
                let u = T.Vertex.make s pos in
                let n = T.Node.make (Var (T.Labeled_var.var output)) (T.Vertex.position v') pos in
                self#log (Trace.shift output v w v' u);
                if not (Gss.contains u stack) then begin
                  stack <- Gss.add u stack;
                  subclasses <- Subclasses.add pos u subclasses;
                  self#order u;
                  self#expand u
                end;
                if not (Gss.contains_edge u v' stack) then begin
                  stack <- Gss.connect u v' (T.Nodes.singleton n) stack;
                  reduce <- Segments.add u v' reduce;
                  shift1 <- Segments.add u v' shift1;
                  forest <- Forest.add n forest;
                end else if not (Forest.mem n forest) then begin
                  stack <- Gss.connect u v' (T.Nodes.add n @@ Gss.node u v' stack) stack;
                  forest <- Forest.add n forest;
                end;
                forest <- Forest.pack n (T.Vars.singleton @@ T.Labeled_var.label output) [] forest))
          (Orders'.find_multiple_or ~default:Orders.empty w orders
           |> Orders.find_multiple_or ~default:T.Vertices.empty (T.Labeled_var.var output)
           |> T.Vertices.to_seq)

      method private prediction v l vars =
        let xs = List.map (fun x -> T.Symbol.Var x) vars in
        Seq.iter (fun w ->
            let xs = List.filter (fun x -> Tables.shift @@ Tables.actions t (T.Vertex.states w) x) xs
            in
            if not (T.Vertices.mem  w processed) && not (List.is_empty xs) then begin
              self#log (Trace.predict v w vars);
              self#actor ~null:false w (Segments.find_multiple w reduce) xs;
              processed <- T.Vertices.add w processed
            end)
          (T.Vertices.to_seq l)

      method private reduce ?filter v l r xs =
        Seq.iter (fun (w, ns) ->
            Tables.goto t (T.Vertex.states w) (Var (T.Labeled_var.var r.output)) |>
            T.States.iter (fun s ->
                let pos = T.Vertex.position v in
                let u = T.Vertex.make s pos in
                let n = T.Node.make (Var (T.Labeled_var.var r.output)) (T.Vertex.position w) pos in
                self#log (Trace.reduce r.output v w u);
                if not (Gss.contains u stack) then begin
                  stack <- Gss.add u stack;
                  subclasses <- Subclasses.add pos u subclasses;
                  self#order u;
                  match r.strategy with
                  | Null -> self#actor ~null:true u (T.Vertices.singleton w) xs
                  | _ -> ()
                end;
                if not (Gss.contains_edge u w stack) then begin
                  stack <- Gss.connect u w (T.Nodes.singleton n) stack;
                  forest <- Forest.add n forest;
                  match r.strategy with
                  | Null -> ()
                  | _ -> self#actor ~null:false u (T.Vertices.singleton w) xs
                end else if not (Forest.mem n forest) then begin
                  stack <- Gss.connect u w (T.Nodes.add n @@ Gss.node u w stack) stack;
                  forest <- Forest.add n forest;
                  match r.strategy with
                  | Null -> ()
                  | _ -> self#actor ~null:false ~filter:n u (T.Vertices.singleton w) xs
                end;
                orders <- Orders'.add_multiple v (Orders'.find_multiple_or ~default:Orders.empty u orders) orders; (* add orders in any case *)
                List.iter (fun ns' ->
                    forest <- Forest.pack n (T.Vars.singleton @@ T.Labeled_var.label r.output) (ns @ ns') forest)
                  (self#enumerate pos r.reminder)))
          (self#find_paths ?filter v l r.strategy)

      method private expand v =
        Tables.goto t (T.Vertex.states v) Delegate |>
        T.States.iter (fun s ->
            (let pos = T.Vertex.position v in
             let u = T.Vertex.make s pos in
             self#log (Trace.expand v u);
             if not (Gss.contains u stack) then begin
               stack <- Gss.add u stack;
               subclasses <- Subclasses.add pos u subclasses;
             end;
             if not (Gss.contains_edge u v stack) then begin
               stack <- Gss.connect u v T.Nodes.empty stack;
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
            if Tables.load @@ Tables.actions t (T.Vertex.states w) x then
              self#actor ~null:false w l [x])
          (Segments.to_seq_multiple shift0);
        (*Fmt.pr "AFTER %a@," (T.Vertex_to.pp (Fmt.parens T.Vertices.pp)) read0;*)
        Seq.iter (fun (w, l) ->
            Seq.iter (fun v ->
                if T.Vars.exists (fun x ->
                    Tables.shift @@ Tables.actions t (T.Vertex.states v) (T.Symbol.Var x))
                    (Tables.predictions @@ Tables.actions t (T.Vertex.states w) x)
                then
                  Tables.goto t (T.Vertex.states w) x |>
                  T.States.iter (fun s ->
                      let pos = succ @@ T.Vertex.position w in
                      let u = T.Vertex.make s pos in
                      self#log (Trace.read x v w u );
                      if not (Gss.contains u stack) then begin
                        stack <- Gss.add u stack;
                        subclasses <- Subclasses.add pos u subclasses;
                      end;
                      if not (Gss.contains_edge u v stack) then begin
                        stack <- Gss.connect u v T.Nodes.empty stack;
                        read1 <- Segments.add u v read1
                      end))
              (T.Vertices.to_seq l))
          (Segments.to_seq_multiple read0);
      end

      method forest =
        forest

      method trace =
        List.rev trace

      method accept =
        Seq.exists (fun (w, _) ->
            Tables.accept t (T.Vertex.states w))
          (Segments.to_seq_multiple shift1)

      method to_dot =
        let subgraphs = Subclasses.fold_multiple (fun position subclass subgraphs ->
            let nodes =
              subclass
              |> T.Vertices.to_list
              |> List.map (fun v ->
                  Dot.(node (T.Vertex.to_id v) ~attrs:([
                      "xlabel" => String (Int.to_string @@ T.Vertex.position v);
                      "label" => String (Fmt.to_to_string T.State.pp (T.Vertex.states v));
                    ] @ if Segments.domain_mem v reduce then [
                      "shape" => String "octagon";
                    ] else [])))
            in
            let attrs = Dot.[
                Attr ("label" => HTML ("U<SUB>" ^ (Int.to_string position) ^ "</SUB>"));
              ]
            in
            Dot.Subgraph ("cluster_" ^ (Int.to_string position), attrs @ nodes) :: subgraphs)
            [] subclasses
        in
        let edges =
          Gss.G.labeled_edges stack
          |> List.of_seq
          |> List.map (fun (v1, v2, n) ->
              Dot.(edge Directed (T.Vertex.to_id v1) (T.Vertex.to_id v2) ~attrs:([
                  "label" => String (Fmt.str "%a" (T.Nodes.pp) n)
                ] @ if T.Nodes.is_empty n then [
                  "style" => String "dotted";
                ] else [])))
        in
        let orders =
          Gss.G.vertices stack
          |> List.of_seq
          |> List.concat_map (fun w ->
              List.of_seq @@
              Seq.map (fun (x, v) ->
                  Dot.(edge Directed (T.Vertex.to_id w) (T.Vertex.to_id v) ~attrs:[
                      "label" => String (Fmt.str "%a" (T.Var.pp) x);
                      "style" => String "dashed";
                      "arrowhead" => String "onormal";
                    ]))
                (Orders.to_seq (Orders'.find_multiple_or ~default:Orders.empty w orders)))
        in
        let attrs = Dot.[
            Attr ("rankdir" => Id "RL");
            Attr ("newrank" => Id "true")
          ]
        in
        Dot.((Digraph, "g", attrs @ subgraphs @ edges @ orders))
    end
end

