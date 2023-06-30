open Te_bot
open! Prelude
module T = Types

module Gss = struct
  module G = T.Vertex_graph
  type t = (unit, unit) G.t
  let singleton v = G.singleton v ()

  let push u v g =
    assert (not (G.vertex_mem u g));
    g
    |> G.add u ()
    |> G.connect u v ()

  let connect u v g =
    G.connect u v () g

  let contains u g =
    G.vertex_mem u g

  let contains_edge u v g =
    G.edge_mem u v g

  let adjacent v g =
    G.adjacent v g
    |> Seq.map (fun (w, _) -> w)
    |> T.Vertices.of_seq
end

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

  type symbol = Null | Code of T.Code.t | Var of T.Var.t

  val start: t -> states 
  val actions: t -> states -> symbol -> actions 
  val goto: t -> states -> symbol -> states option
  val back: t -> state * state -> state -> (state * state) option

  val shift: actions -> bool
  val matches: actions -> T.Labeled_vars.t
  val predictions: actions -> T.Labeled_vars.t
  val null: actions -> T.Labeled_vars.t
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

    method private successors v l d =
      if Int.equal d 0
      then T.Vertices.singleton v
      else T.Vertices.fold (fun v' -> T.Vertices.union @@ self#successors v' (Gss.adjacent v' stack) (pred d)) T.Vertices.empty l

    method private scan_back v l p =
      Fmt.pr "SB %a %a@," T.Vertex.pp v (Fmt.parens T.Vertices.pp) l;
      if T.Vertices.is_empty l
      then T.Vertices.singleton v
      else T.Vertices.fold (fun v' -> 
          T.Vertices.union @@
          T.States.fold (fun s ->
              T.Vertices.union @@
              match Tables.back t p s with
              | Some p' -> self#scan_back v' (Gss.adjacent v' stack) p'
              | None -> T.Vertices.singleton v)
            T.Vertices.empty (T.Vertex.state v'))
          T.Vertices.empty l

    method private actor ~null v l xs = 
      Fmt.pr "actor %b %a %a %a@," null T.Vertex.pp v T.Vertices.pp l (Fmt.list self#pp_symbol) xs;
      List.iter (fun x ->
          let a = Tables.actions t (T.Vertex.state v) x in
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
          Seq.iter (fun output ->
              self#null v output xs)
            (T.Labeled_vars.to_seq @@ Tables.null a);
          Seq.iter (fun output ->
              self#shift v l output)
            (T.Labeled_vars.to_seq @@ Tables.matches a))
        xs

    method private null v output xs =
      (match Tables.goto t (T.Vertex.state v) (Var (T.Labeled_var.var output)) with
       | Some s ->
         let pos = T.Vertex.position v in
         let u = T.Vertex.make s pos in
         Fmt.pr "NULL %a %a -> %a %a@," T.Vertex.pp v T.Vertex.pp u T.Labeled_var.pp output (Fmt.list self#pp_symbol) xs;
         if Gss.contains u stack && not (Gss.contains_edge u v stack) then begin
           stack <- Gss.connect u v stack
         end else if not (Gss.contains u stack) then begin
           subclasses <- Subclasses.add pos u subclasses;
           stack <- Gss.push u v stack;
           self#actor ~null:true u (T.Vertices.singleton v) xs;
           orders <- Orders'.add_multiple v (Orders'.find_multiple_or ~default:Orders.empty u orders) orders
         end
       | None -> ())

    method private shift v l output =
      Seq.iter (fun w ->
          Seq.iter (fun v' ->
              (match Tables.goto t (T.Vertex.state v') (Var (T.Labeled_var.var output)) with
               | Some s ->
                 let pos = T.Vertex.position v in
                 let u = T.Vertex.make s pos in
                 Fmt.pr "SHIFT %a %a %a %a -> %a@," T.Vertex.pp v T.Vertex.pp w T.Vertex.pp v' T.Vertex.pp u T.Labeled_var.pp output;
                 if Gss.contains u stack && not (Gss.contains_edge u v' stack) then begin
                   stack <- Gss.connect u v' stack;
                   reduce <- Segments.add u v' reduce;
                   self#expand u
                 end else if not (Gss.contains u stack) then begin
                   reduce <- Segments.add u v' reduce;
                   subclasses <- Subclasses.add pos u subclasses;
                   stack <- Gss.push u v' stack;
                   self#expand u
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
      let ws = T.Vertices.to_seq @@ match r.strategy with
        | Fixed d -> self#successors v l d
        | Scan p -> self#scan_back v l p
      in
      Seq.iter (fun w ->
          (match Tables.goto t (T.Vertex.state w) (Var (T.Labeled_var.var r.output)) with
           | Some s ->
             let pos = T.Vertex.position v in
             let u = T.Vertex.make s pos in
             Fmt.pr "REDUCE %a %a %a -> %a@," T.Vertex.pp v T.Vertex.pp w T.Vertex.pp u T.Labeled_var.pp r.output;
             if Gss.contains u stack && not (Gss.contains_edge u w stack) then begin
               stack <- Gss.connect u w stack;
               self#actor ~null:false u (T.Vertices.singleton w) xs;
               orders <- Orders'.add_multiple v (Orders'.find_multiple_or ~default:Orders.empty u orders) orders
             end else if not (Gss.contains u stack) then begin
               subclasses <- Subclasses.add pos u subclasses;
               stack <- Gss.push u w stack;
               self#actor ~null:false u (T.Vertices.singleton w) xs;
               orders <- Orders'.add_multiple v (Orders'.find_multiple_or ~default:Orders.empty u orders) orders
             end
           | None -> ()))
        ws

    method private expand v =
      match Tables.goto t (T.Vertex.state v) Null with
      | Some s ->
        (let pos = T.Vertex.position v in
         let u = T.Vertex.make s pos in
         Fmt.pr "EXPAND %a %a@," T.Vertex.pp v T.Vertex.pp u;
         if Gss.contains u stack then begin
           read1 <- Segments.add u v read1;
           stack <- Gss.connect u v stack
         end else begin
           read1 <- Segments.add u v read1;
           subclasses <- Subclasses.add pos u subclasses;
           stack <- Gss.push u v stack
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
              match Tables.goto t (T.Vertex.state w) x with
              | Some s ->
                let pos = succ @@ T.Vertex.position w in
                let u = T.Vertex.make s pos in
                Fmt.pr "READ %a %a %a -> %a@," T.Vertex.pp w T.Vertex.pp v T.Vertex.pp u self#pp_symbol x;
                if Gss.contains u stack then begin
                  read1 <- Segments.add u v read1;
                  stack <- Gss.connect u v stack
                end else begin
                  read1 <- Segments.add u v read1;
                  subclasses <- Subclasses.add pos u subclasses;
                  stack <- Gss.push u v stack
                end
              | None -> ())
            (T.Vertices.to_seq l))
        (Segments.to_seq_multiple read0)
    end

    method to_dot =
       let subgraphs = Subclasses.fold_multiple (fun position subclass subgraphs ->
        let nodes =
          subclass
          |> T.Vertices.to_list
          |> List.map (fun v ->
              Dot.(node (T.Vertex.to_id v) ~attrs:[
                  "xlabel" => String (Int.to_string @@ T.Vertex.position v);
                  "label" => String (Fmt.to_to_string T.States.pp (T.Vertex.state v));
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
              "label" => String (Unit.to_string n)
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

