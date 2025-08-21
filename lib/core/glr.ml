open Te_bot
open! Prelude
module T = Types
open Driver

module Scanner = struct
  type t =
    {
      pos: int;
      state: T.State.t;
      vars: T.Vars.t;
    }
end

module type TABLES = sig
  type t
  type actions = T.Actions.t
  type state := T.State.t

  type symbol := T.Symbol.t

  val start_parser: t -> state
  val start_scanner: t -> state
  val actions: t -> state -> symbol -> actions
  val goto: t -> state -> symbol -> state option
  val matches: t -> state -> T.Labeled_vars.t
  val predictions: t -> state -> T.Vars.t
  val accept: t -> state -> bool
  val valid_lookahead: t -> state -> T.Vars.t

  val shift: actions -> bool
  val null: actions -> T.Reductions.t
  val reduce: actions -> T.Reductions.t

  val early_stop: t -> T.Var.t -> (T.Symbol.t -> T.Symbol.t -> T.Symbol.t -> T.Symbol.t -> bool)
end

module Subclasses = Multimap.Make3(Balanced_binary_tree.Map.Size(Int))(T.Vertices)
module Segments = Multimap.Make3(T.Vertex_to)(T.Vertices)

module Make(Tables: TABLES) = struct

  class glr (t: Tables.t): driver =
    let bottom = T.Vertex.make (Tables.start_parser t) 0 in object(self)
      val mutable stack: Gss.t = Gss.singleton bottom
      val mutable forest = Forest.empty
      val mutable subclasses = Subclasses.singleton 0 bottom

      val mutable shift0 = Segments.empty
      val mutable shift1 = Segments.singleton bottom bottom
      val mutable scanner = Scanner.{
          pos = 0;
          state = Tables.start_scanner t;
          vars = Tables.valid_lookahead t (Tables.start_parser t);
        }
      val mutable buffer = [|None; None; None|]
      val mutable trace: Trace.t = []

      method private log action =
        trace <- action :: trace

      method private successors v ns paths d =
        if Int.equal d 0
        then Paths.singleton (v, ns)
        else Paths.concat_map (fun (v', ns') ->
            self#successors v' ns' (Gss.adjacent v' ns' stack) (pred d))
            paths

      method private enumerate pos xss =
        List.map (fun xs ->
            List.map (fun x -> T.Node.make (Var x) pos pos) xs)
          xss

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
        | Scan _ -> failwith "Not supported"
        | Null -> Paths.singleton (v, [])

      method private parse_actor ~null ?filter v l pos matches =
        let xs = List.map (fun x -> T.Symbol.Var x)
            (T.Vars.to_list @@ T.Labeled_vars.vars matches) 
        in
        let a = List.fold_left (fun acc x ->
            T.Actions.union (Tables.actions t (T.Vertex.states v) x) acc)
            T.Actions.empty xs
        in
        if not null then
          T.Reductions.iter (fun r ->
              self#reduce ?filter v l r pos matches)
            (Tables.reduce a);
        T.Reductions.iter (fun r ->
            self#reduce v T.Vertices.empty r pos matches)
          (Tables.null a);
        T.Labeled_vars.iter (fun match_ ->
            self#load v pos match_)
          matches

      method private load v pos match_ =
        let x = T.Symbol.Var (T.Labeled_var.var match_) in
        match Tables.goto t (T.Vertex.states v) x with
        | Some s ->
          let u = T.Vertex.make s pos in
          let n = T.Node.make x (T.Vertex.position v) pos in
          self#log (Trace.load x v u);

          if not (Gss.contains u stack) then begin
            stack <- Gss.add u stack;
            subclasses <- Subclasses.add pos u subclasses;
          end;
          if not (Gss.contains_edge u v stack) then begin
            stack <- Gss.connect u v (T.Nodes.singleton n) stack;
            shift1 <- Segments.add u v shift1;
            forest <- Forest.add n forest;
          end;
          forest <- Forest.pack n (T.Vars.singleton @@ T.Labeled_var.label match_) [] forest
        | None -> ()


      method private reduce ?filter v l r pos' matches =
        List.iter (fun (w, ns) ->
            match Tables.goto t (T.Vertex.states w) (Var (T.Labeled_var.var r.output)) with
            | Some s ->
              let pos = T.Vertex.position v in
              let u = T.Vertex.make s pos in
              let n = T.Node.make (Var (T.Labeled_var.var r.output)) (T.Vertex.position w) pos in
              self#log (Trace.reduce r.output v w u);

              if not (Gss.contains u stack) then begin
                stack <- Gss.add u stack;
                subclasses <- Subclasses.add pos u subclasses;
                match r.strategy with
                | Null -> self#parse_actor ~null:true u (T.Vertices.singleton w) pos' matches
                | _ -> ()
              end;
              if not (Gss.contains_edge u w stack) then begin
                stack <- Gss.connect u w (T.Nodes.singleton n) stack;
                forest <- Forest.add n forest;
                match r.strategy with
                | Null -> ()
                | _ -> self#parse_actor ~null:false u (T.Vertices.singleton w) pos' matches
              end else if not (Forest.mem n forest) then begin
                stack <- Gss.connect u w (T.Nodes.add n @@ Gss.node u w stack) stack;
                forest <- Forest.add n forest;
                match r.strategy with
                | Null -> ()
                | _ -> self#parse_actor ~null:false ~filter:n u (T.Vertices.singleton w) pos' matches
              end;
              List.iter (fun ns' ->
                  forest <- Forest.pack
                      n
                      (T.Vars.singleton @@ T.Labeled_var.label r.output)
                      (ns @ ns')
                      forest)
                (self#enumerate pos r.reminder)
            | None -> ())
          (self#find_paths ?filter v l r.strategy)

      method read x = begin
        let x' = buffer.(0) in
        buffer.(0) <- buffer.(1);
        buffer.(1) <- buffer.(2);
        buffer.(2) <- Some x;

        match x' with
        | Some x' -> begin
          let move scanner =
            match Tables.goto t scanner.Scanner.state x' with
            | Some s ->
              let vars = T.Vars.inter (Tables.predictions t s) scanner.vars in
              let early_stop = T.Vars.exists (fun v ->
                  Tables.early_stop t v x' !!(buffer.(0)) !!(buffer.(1)) !!(buffer.(2)))
                  (Tables.matches t scanner.state
                  |> T.Labeled_vars.vars
                  |> T.Vars.inter scanner.vars)
              in
              if T.Vars.is_empty vars || early_stop
              then None
              else Some Scanner.{pos = succ scanner.pos; state = s; vars}
            | None -> None
          in
          match move scanner with
            | Some scanner' ->
              scanner <- scanner'
            | None ->
              shift0 <- shift1;
              shift1 <- Segments.empty;
              let matches =
                Tables.matches t scanner.state 
                |> T.Labeled_vars.filter (fun (_, var) -> T.Vars.mem var scanner.vars)
              in
              Seq.iter (fun (w, l) ->
                  self#parse_actor ~null:false w l scanner.pos matches)
                (Segments.to_seq_multiple shift0);

              let vars =
                shift1
                |> Segments.to_seq_multiple
                |> Seq.map (fun (v, _) -> Tables.valid_lookahead t (T.Vertex.states v))
                |> Seq.fold_left T.Vars.union T.Vars.empty
              in
              match move Scanner.{pos = scanner.pos; state = Tables.start_scanner t; vars} with
              | Some scanner' -> 
                scanner <- scanner'
              | None -> ()
        end
        | None -> ()
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
        let attrs = Dot.[
            Attr ("rankdir" => Id "RL");
            Attr ("newrank" => Id "true")
          ]
        in
        Dot.((Digraph, "g", attrs @ subgraphs @ edges))
    end
end
