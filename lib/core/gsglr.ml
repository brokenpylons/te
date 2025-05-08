open Te_bot
open! Prelude
module T = Types

module Path = struct
  type t = T.Vertex.t * T.Node.t list
  [@@deriving ord, show]
end
module Paths = struct
 include List
 let singleton x = [x]
 let add x xs = x :: xs
 let empty = []
end

module Gss = struct
  module G = T.Vertex_graph
  type t = (unit, T.Nodes.t) G.t
  let singleton v = G.singleton v ()

  let add u g =
    (*assert (not (G.vertex_mem u g));*)
    G.add u () g

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

module type TABLES = sig
  type t
  type actions = T.Actions.t
  type state := T.State.t
  type state_pair := T.State_pair.t

  type symbol := T.Symbol.t

  val start: t -> state
  val actions: t -> state -> symbol -> actions
  val goto: t -> state -> symbol -> state option
  val orders: t -> state -> T.Vars.t
  val back: t -> state_pair -> state -> symbol -> state_pair option
  val stop: t -> state_pair -> bool
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
      val mutable reduce = Segments.singleton bottom bottom
      val mutable read0 = Segments.empty
      val mutable read1 = Segments.empty
      val mutable shift0 = Segments.empty
      val mutable shift1 = Segments.singleton bottom bottom

      val mutable trace: Trace.t = []

      val mutable count = 0

      initializer
        self#order bottom;
        self#expand bottom

      method private log _action = ()
        (*trace <- action :: trace*)

      method private successors v ns paths d =
        if Int.equal d 0
        then Paths.singleton (v, ns)
        else Paths.concat_map (fun (v', ns') ->
            self#successors v' ns' (Gss.adjacent v' ns' stack) (pred d))
            paths

      method private scan_back ~history v ns paths p =
        (*Fmt.pr "SB %a %a %a@," T.Vertex.pp v T.Vertices.pp history (Fmt.parens Paths.pp) paths;*)
        if T.Vertices.mem v history then
          Paths.empty
        else
          let history = T.Vertices.add v history in
          if Tables.stop t p
          then Paths.singleton (v, ns)
          else Paths.concat_map (fun (v', ns') ->
              let n = match ns' with n :: _ -> n | [] -> assert false in
              let p' = !! (Tables.back t p (T.Vertex.states v') (T.Node.symbol n)) in
              self#scan_back ~history v' ns' (Gss.adjacent v' ns' stack) p')
              paths

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
        List.sort_uniq Path.compare @@ match strategy with
        | Fixed d -> self#successors v [] init d
        | Scan p -> self#scan_back ~history:T.Vertices.empty v [] init p
        | Null -> Paths.singleton (v, [])

      method private scan_actor v l xs =
        let a = List.fold_left (fun acc x ->
            T.Actions.union (Tables.actions t (T.Vertex.states v) x) acc)
            T.Actions.empty xs
        in
        if not (T.Labeled_vars.is_empty @@ Tables.matches a) then
          self#prediction v l (T.Vars.to_list @@ Tables.predictions a);
        T.Labeled_vars.iter (fun output ->
            T.Vertices.iter (fun w ->
                let pos = T.Vertex.position v in
                self#shift pos v w output)
              l)
          (Tables.matches a);

      method private parse_actor ~null ?filter v l xs =
        let a = List.fold_left (fun acc x ->
            T.Actions.union (Tables.actions t (T.Vertex.states v) x) acc)
            T.Actions.empty xs
        in
        if not null then
          T.Reductions.iter (fun r ->
              self#reduce ?filter v l r xs)
            (Tables.reduce a);
        T.Reductions.iter (fun r ->
            self#reduce v T.Vertices.empty r xs)
          (Tables.null a);
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
        match Tables.goto t (T.Vertex.states v) x with
        | Some s ->
          let pos = succ @@ T.Vertex.position v in
          let u = T.Vertex.make s pos in
          let n = T.Node.make x (T.Vertex.position v) pos in
          self#log (Trace.load x v u);

          if not (Gss.contains u stack) then begin
            stack <- Gss.add u stack;
            (*subclasses <- Subclasses.add pos u subclasses;*)
            self#order u;
            self#expand u
          end;
          if not (Gss.contains_edge u v stack) then begin
            stack <- Gss.connect u v (T.Nodes.singleton n) stack;
            reduce <- Segments.add u v reduce;
            shift1 <- Segments.add u v shift1;
            forest <- Forest.add n forest;
          end
        | None -> ()


      method private shift pos v w output =
        T.Vertices.iter (fun v' ->
            match Tables.goto t (T.Vertex.states v') (Var (T.Labeled_var.var output)) with
            | Some s ->
              let u = T.Vertex.make s pos in
              let n = T.Node.make (Var (T.Labeled_var.var output)) (T.Vertex.position v') pos in
              self#log (Trace.shift output v w v' u);
              if not (Gss.contains u stack) then begin
                stack <- Gss.add u stack;
                (*subclasses <- Subclasses.add pos u subclasses;*)
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
              forest <- Forest.pack n (T.Vars.singleton @@ T.Labeled_var.label output) [] forest
            | None -> ())
          (Orders'.find_multiple_or_empty w orders
           |> Orders.find_multiple_or_empty (T.Labeled_var.var output))

      method private prediction v l vars =
        let xs = List.map (fun x -> T.Symbol.Var x) vars in
        T.Vertices.iter (fun w ->
            let xs = List.filter (fun x ->
                Tables.shift @@ Tables.actions t (T.Vertex.states w) x)
                xs
              in
            if Segments.domain_mem w reduce && not (List.is_empty xs) then begin
              self#log (Trace.predict v w vars);
              self#parse_actor ~null:false w (Segments.find_multiple w reduce) xs;
              reduce <- Segments.remove_multiple w reduce
            end)
          l

      method private reduce ?filter v l r xs =
        List.iter (fun (w, ns) ->
            match Tables.goto t (T.Vertex.states w) (Var (T.Labeled_var.var r.output)) with
            | Some s ->
              let pos = T.Vertex.position v in
              let u = T.Vertex.make s pos in
              let n = T.Node.make (Var (T.Labeled_var.var r.output)) (T.Vertex.position w) pos in
              self#log (Trace.reduce r.output v w u);

              if not (Gss.contains u stack) then begin
                stack <- Gss.add u stack;
                (*subclasses <- Subclasses.add pos u subclasses;*)
                self#order u;
                match r.strategy with
                | Null -> self#parse_actor ~null:true u (T.Vertices.singleton w) xs
                | _ -> ()
              end;
              if not (Gss.contains_edge u w stack) then begin
                stack <- Gss.connect u w (T.Nodes.singleton n) stack;
                forest <- Forest.add n forest;
                match r.strategy with
                | Null -> ()
                | _ -> self#parse_actor ~null:false u (T.Vertices.singleton w) xs
              end else if not (Forest.mem n forest) then begin
                stack <- Gss.connect u w (T.Nodes.add n @@ Gss.node u w stack) stack;
                forest <- Forest.add n forest;
                match r.strategy with
                | Null -> ()
                | _ -> self#parse_actor ~null:false ~filter:n u (T.Vertices.singleton w) xs
              end;
              orders <- Orders'.add_multiple v (Orders'.find_multiple_or_empty u orders) orders;
              List.iter (fun ns' ->
                  forest <- Forest.pack 
                      n
                      (T.Vars.singleton @@ T.Labeled_var.label r.output)
                      (ns @ ns')
                      forest)
                (self#enumerate pos r.reminder)
            | None -> ())
          (self#find_paths ?filter v l r.strategy)

      method private expand v =
        match Tables.goto t (T.Vertex.states v) Delegate with
        | Some s ->
          (let pos = T.Vertex.position v in
           let u = T.Vertex.make s pos in
           self#log (Trace.expand v u);
           read1 <- Segments.add u v read1
           (*if not (Gss.contains u stack) then begin
             stack <- Gss.add u stack;
             (*subclasses <- Subclasses.add pos u subclasses;*)
             end;
             if not (Gss.contains_edge u v stack) then begin
             stack <- Gss.connect u v T.Nodes.empty stack;
             read1 <- Segments.add u v read1
             end*))
        | None -> ()

      method read x = begin
        (*Fmt.pr "@,COMPLETE %a@," T.Symbol.pp x;*)
        (*Fmt.pr "R %a@," (T.Vertex_to.pp (Fmt.parens T.Vertices.pp)) read1;*)
        Seq.iter (fun (w, l) ->
            self#scan_actor w l [x])
          (Segments.to_seq_multiple read1);

        read0 <- read1;
        read1 <- Segments.empty;
        shift0 <- shift1;
        shift1 <- Segments.empty;
        (*Fmt.pr "AFTER %a@," (T.Vertex_to.pp (Fmt.parens T.Vertices.pp)) read0;*)
        Seq.iter (fun (w, l) ->
            if Tables.load @@ Tables.actions t (T.Vertex.states w) x then
              self#parse_actor ~null:false w l [x])
          (Segments.to_seq_multiple shift0);
        (*Fmt.pr "AFTER %a@," (T.Vertex_to.pp (Fmt.parens T.Vertices.pp)) read0;*)

        Seq.iter (fun (w, l) ->
            T.Vertices.iter (fun v ->
                (*if T.Vars.exists (fun x ->
                    Tables.shift @@ Tables.actions t (T.Vertex.states v) (T.Symbol.Var x))
                    (Tables.predictions @@ Tables.actions t (T.Vertex.states w) x)
                  then*)
                match Tables.goto t (T.Vertex.states w) x with
                | Some s ->
                  let pos = succ @@ T.Vertex.position w in
                  let u = T.Vertex.make s pos in
                  self#log (Trace.read x v w u );
                  read1 <- Segments.add u v read1
                  (*if not (Gss.contains u stack) then begin
                    stack <- Gss.add u stack;
                    (*subclasses <- Subclasses.add pos u subclasses;*)
                    end;
                    if not (Gss.contains_edge u v stack) then begin
                    stack <- Gss.connect u v T.Nodes.empty stack;
                    read1 <- Segments.add u v read1
                    end*)
                | None -> ())
              l)
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

      method status =
        Fmt.pr "%i@." count;

      method to_dot =
        let subgraphs = Subclasses.fold_multiple (fun position subclass subgraphs ->
            let nodes =
              subclass
              |> T.Vertices.to_list
              |> List.map (fun v ->
                  Dot.(node (T.Vertex.to_id v) ~attrs:([
                      "xlabel" => String (Int.to_string @@ T.Vertex.position v);
                      "label" => String (Fmt.to_to_string T.State.pp (T.Vertex.states v));
                    ])))
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
                (Orders.to_seq (Orders'.find_multiple_or_empty w orders)))
        in
        let attrs = Dot.[
            Attr ("rankdir" => Id "RL");
            Attr ("newrank" => Id "true")
          ]
        in
        Dot.((Digraph, "g", attrs @ subgraphs @ edges @ orders))
    end
end

