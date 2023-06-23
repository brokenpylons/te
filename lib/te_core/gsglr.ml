open Te_bot
open! Prelude
module T = Types

module Gss = struct
  module G = T.Vertex_graph
  type t = (unit, unit) G.t
  let singleton v = G.singleton v ()

  let push u v g =
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

  type symbol = Null | Code of T.Code.t | Var of T.Var.t

  val start: t -> T.State.t
  val actions: t -> T.State.t -> symbol -> actions 
  val goto: t -> T.State.t -> symbol -> T.State.t option

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
    val mutable read = Segments.singleton bottom bottom
    val mutable subclasses = Subclasses.singleton 0 bottom
    val mutable orders = Orders'.empty

    initializer
      self#expand bottom

    method private successors v l d =
      if Int.equal d 0
      then T.Vertices.singleton v
      else T.Vertices.fold (fun v' -> T.Vertices.union @@ self#successors v' (Gss.adjacent v' stack) (pred d)) T.Vertices.empty l

    method private actor ~null v l xs = 
      List.iter (fun x ->
          let a = Tables.actions t (T.Vertex.state v) x in
          (if Tables.shift a then
             match x with
             | Var x ->
               orders <- Orders'.add_multiple v (Orders.singleton x v) orders
             | _ -> ());
          if not (T.Labeled_vars.is_empty @@ Tables.matches a) then begin
            self#prediction l (List.map (fun output -> Tables.Var (T.Labeled_var.var output)) (T.Labeled_vars.to_list @@ Tables.predictions a));
          end;
          if not null then
            T.Reductions.iter (fun r ->
                self#reduce v l r xs) (Tables.reduce a);
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
         if Gss.contains u stack then
           stack <- Gss.push u v stack
         else begin
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
                 if Gss.contains u stack then
                   stack <- Gss.connect u v' stack
                 else begin
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
          self#actor ~null:false w (Segments.find_multiple w reduce) xs)
        (T.Vertices.to_seq l)

    method private reduce v l r xs =
      match r.strategy with
      | Fixed d ->
        Seq.iter (fun w ->
            (match Tables.goto t (T.Vertex.state w) (Var (T.Labeled_var.var r.output)) with
             | Some s ->
               let pos = T.Vertex.position v in
               let u = T.Vertex.make s pos in
               if Gss.contains u stack && not (Gss.contains_edge u w stack) then begin
                 stack <- Gss.connect u w stack;
                 self#actor ~null:false u (T.Vertices.singleton w) xs;
                 orders <- Orders'.add_multiple v (Orders'.find_multiple_or ~default:Orders.empty u orders) orders
               end else begin
                 subclasses <- Subclasses.add pos u subclasses;
                 stack <- Gss.push u w stack;
                 self#actor ~null:false u (T.Vertices.singleton w) xs;
                 orders <- Orders'.add_multiple v (Orders'.find_multiple_or ~default:Orders.empty u orders) orders
               end
             | None -> ()))
          (T.Vertices.to_seq @@ self#successors v l d)
      | _ -> failwith "Not implemented"

    method private expand v =
      match Tables.goto t (T.Vertex.state v) Null with
      | Some s ->
        (let pos = T.Vertex.position v in
         let u = T.Vertex.make s pos in
         read <- Segments.add u v read;
         subclasses <- Subclasses.add pos u subclasses;
         stack <- Gss.push u v stack)
      | None -> ()

    method read x =
      Seq.iter (fun (w, l) ->
          self#actor ~null:false w l [x];
          Seq.iter (fun v ->
              match Tables.goto t (T.Vertex.state w) x with
              | Some s ->
                let pos = succ @@ T.Vertex.position w in
                let u = T.Vertex.make s pos in
                if Gss.contains u stack then
                  stack <- Gss.connect u v stack
                else begin
                  read <- Segments.add u v read;
                  subclasses <- Subclasses.add pos u subclasses;
                  stack <- Gss.push u v stack
                end
              | None -> ())
            (T.Vertices.to_seq l))
        (Segments.to_seq_multiple read)

    method to_dot =
       let subgraphs = Subclasses.fold_multiple (fun position subclass subgraphs ->
        let nodes =
          subclass
          |> T.Vertices.to_list
          |> List.map (fun v ->
              Dot.(node (T.Vertex.to_id v) ~attrs:[
                  "xlabel" => String (Int.to_string @@ T.Vertex.position v);
                  "label" => String (Fmt.to_to_string T.State.pp (T.Vertex.state v));
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
        Attr ("rankdir" => Id "RL")
      ]
    in
    Dot.((Digraph, "g", attrs @ subgraphs @ edges))
  end
end

