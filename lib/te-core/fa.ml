open Te_bot
open! Prelude
module T = Types

module type LITS = sig
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int
end

module type LABELS = sig
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int
end

module type FA = sig
  module Start: sig
    type single = S
    type multiple = M

    type 'a t =
      | Single: T.State.t -> single t
      | Multiple: T.States.t -> multiple t
    val equal: 'a t -> 'a t -> bool
    val compare: 'a t -> 'a t -> int
  end

  type labels
  type lits
  type 'a t = {
    start: 'a Start.t;
    final: T.States.t;
    graph: (labels, lits) T.State_graph.t;
  }

  val start: Start.single t -> T.State.t
  val start_multiple: 'a t -> T.States.t
  val final: 'a t -> T.States.t
  val skeleton: 'a t -> (labels, lits) T.State_graph.t
  val transitions: 'a t -> (T.State.t * T.State.t * lits) Seq.t
  val link: ?merge:(lits -> lits -> lits) -> T.States.t -> T.States.t -> lits -> 'a t -> 'a t

  val sum: 'a t -> 'b t -> Start.multiple t
  val empty: Start.multiple t

  module Gen(Index: T.State_graph.INDEX): sig
    val unfold_multiple: supply:(T.State.t Supply.t) -> ?merge:(lits -> lits -> lits) -> (T.State.t -> Index.elt -> bool * labels * (lits * Index.elt) Seq.t) -> Index.elt list -> Start.multiple t

    val unfold: supply:(T.State.t Supply.t) -> ?merge:(lits -> lits -> lits) -> (T.State.t -> Index.elt -> bool * labels * (lits * Index.elt) Seq.t) -> Index.elt -> Start.single t
  end
  val to_dot: string_of_labels:(labels -> string) -> string_of_lits:(lits -> string) -> 'a t -> Dot.graph
end

module Make(Labels: LABELS)(Lits: LITS): FA with type labels = Labels.t and type lits = Lits.t = struct
  module Start = struct
    module Via = struct
      type t =
        | Single of T.State.t
        | Multiple of T.States.t
      [@@deriving ord, eq]
    end

    type single = S
    type multiple = M

    type 'a t =
      | Single: T.State.t -> single t
      | Multiple: T.States.t -> multiple t

    let via: type a. a t -> Via.t = function
      | Single x -> Via.Single x
      | Multiple x -> Via.Multiple x

    let equal: type a. a t -> a t -> bool = fun x y ->
      Via.equal (via x) (via y)

    let compare: type a. a t -> a t -> int = fun x y ->
      Via.compare (via x) (via y)
  end

  type labels = Labels.t
  type lits = Lits.t
  type 'a t = {
    start: 'a Start.t;
    final: T.States.t;
    graph: (labels, lits) T.State_graph.t;
  }

  let start m =
    match m.start with
    | Single start -> start

  let start_multiple: type a. a t -> T.States.t = fun m -> 
    match m.start with
    | Single start -> T.States.singleton start
    | Multiple start -> start

  let final m =
    m.final

  let skeleton m = m.graph

  let empty =
    {
      start = Multiple T.States.empty;
      final = T.States.empty;
      graph = T.State_graph.empty;
    }

  let sum: type a b. a t -> b t -> Start.multiple t = fun m1 m2 ->
    {
      start = Start.Multiple (T.States.union (start_multiple m1) (start_multiple m2));
      final = T.States.union m1.final m2.final;
      graph = T.State_graph.sum m1.graph m2.graph;
    }

  let link ?(merge = Fun.const) f t ls m =
    {
      m with
      graph = T.State_graph.link_with merge (T.States.to_seq f) (T.States.to_seq t) ls m.graph;
    }

  let transitions m =
    T.State_graph.labeled_edges m.graph

  module Gen(Index: T.State_graph.INDEX) = struct
    module Graph_gen = T.State_graph.Gen(Index)

    let unfold_step ~final f q x =
      let (is_final, vl, y) = f q x in
      if is_final then final := T.States.add q !final;
      (vl, y)

    let unfold_multiple ~supply ?merge f start =
      let final = ref T.States.empty in
      let start, graph = Graph_gen.unfold_multiple ~supply ?merge (unfold_step ~final f) start in
      {
        start = Multiple (T.States.of_list start);
        final = !final;
        graph;
      }

    let unfold ~supply ?merge f start =
      let final = ref T.States.empty in
      let start, graph = Graph_gen.unfold ~supply ?merge (unfold_step ~final f) start in
      {
        start = Single start;
        final = !final;
        graph
      }
  end

  let to_dot ~string_of_labels ~string_of_lits a =
    let node q l last labels = Dot.(node (T.State.to_id q) ~attrs:[
        "label" => String l;
        "peripheries" => String (if last then "2" else "1");
        "xlabel" => String labels
      ])
    and edge q0 q1 l = Dot.(edge Directed (T.State.to_id q0) (T.State.to_id q1) ~attrs:[
        "label" => String l
      ])
    and start_edges =
      start_multiple a
      |> T.States.to_seq
      |> Seq.map (fun q0 ->
          Dot.(edge Directed (String "start") (T.State.to_id q0)))
    and start_node = Seq.return
        Dot.(node (String "start") ~attrs:[
            "style" => String "invis"
          ])
    in
    let nodes = Seq.map (fun (q, l) -> 
        (node q (Fmt.to_to_string T.State.pp q) (T.States.mem q a.final) (string_of_labels l)))
        (T.State_graph.labeled_vertices a.graph) 
    in
    let edges = Seq.map (fun (q0, q1, ss) ->
        edge q0 q1 (string_of_lits ss))
        (T.State_graph.labeled_edges a.graph) 
    in
    Dot.(Digraph, "g", List.of_seq @@ Seq.(nodes @ edges @ start_node @ start_edges))
end
