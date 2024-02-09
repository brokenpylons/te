open Te_bot
open! Prelude
module T = Types
module G = T.State_graph

module type STATE = sig
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val pp: t Fmt.t
  val to_id: t -> Dot.id
end

module type STATES = sig
  type t
  type elt
  include Set.CORE with type elt := elt and type t := t
  include Set.SEQUENTIAL with type elt := elt and type t := t
  val union: t -> t -> t
end

module type S0 = sig
  type state
  type states

  module Start: sig
    type single = S
    type multiple = M

    type 'a t =
      | Single: state -> single t
      | Multiple: states -> multiple t
    val equal: 'a t -> 'a t -> bool
    val compare: 'a t -> 'a t -> int
  end

  type ('labels, 'lits) graph
  type ('a, 'labels, 'lits) t = {
    start: 'a Start.t;
    final: states;
    graph: ('labels, 'lits) graph;
  }

  val start: (Start.single, _, _) t -> state
  val start_multiple: _ t -> states
  val final: _ t -> states
  val is_final: state -> _ t -> bool
  val states: _ t -> state Seq.t
  val state_mem: state -> _ t -> bool
  val transitions: (_, _, 'lits) t -> (state * state * 'lits) Seq.t
  val adjacency_list: (_, 'labels, 'lits) t -> ((state * 'labels) * (state * 'lits) Seq.t) Seq.t
  val labels: state -> ('a, 'labels, 'lits) t -> 'labels
  val lits: state -> state -> ('a, 'labels, 'lits) t -> 'lits
  val all_lits: (_, _, 'lits) t -> 'lits Seq.t
  val adjacent: state -> (_, _, 'lits) t -> (state * 'lits) Seq.t
  val merge: merge_labels:('labels -> 'labels -> 'labels) -> merge_lits:('lits -> 'lits -> 'lits) -> (_, 'labels, 'lits) t -> (_, 'labels, 'lits) t -> (Start.multiple, 'labels, 'lits) t
  val sum:  ('a, 'labels, 'lits) t -> ('b, 'labels, 'lits) t -> (Start.multiple, 'labels, 'lits) t
  val empty: (Start.multiple, 'labels, 'lits) t
  val homomorphism: ('lits -> 'lits) -> ('a, 'labels, 'lits) t -> ('a, 'labels, 'lits) t
  val states_labels: ('a, 'labels, 'lits) t -> (state * 'labels) Seq.t
  val map_states_labels: (state -> 'labels1 -> 'labels2) -> ('a, 'labels1, 'lits) t -> ('a, 'labels2, 'lits) t

  val rev: (_, 'labels, 'lits) t -> (Start.multiple, 'labels, 'lits) t

  val unfold:
    ?merge: ('lits -> 'lits -> 'lits) ->
    (state -> 'seed -> bool * 'labels * ('lits * state * 'seed) Seq.t) -> state -> 'seed -> (Start.single, 'labels, 'lits) t

  val digraph: (state -> 'labels -> 'seed) ->
    ('seed -> 'labels -> ('lits * (unit -> 'seed)) Seq.t -> 'seed) ->
    (Start.single, 'labels, 'lits) t ->
    (state -> 'seed)

  val dijkstra: (state -> 'labels -> 'seed) ->
    ('seed -> 'labels -> ('lits * (unit -> 'seed)) Seq.t -> 'seed) ->
    (Start.single, 'labels, 'lits) t ->
    (state -> 'seed)

  val fold: eq:('seed -> 'seed -> bool) ->
    (state -> 'labels -> 'seed) ->
    ('seed -> 'labels -> ('lits *  'seed) Seq.t -> 'seed) ->
    (_, 'labels, 'lits) t ->
    (state -> 'seed)

  val to_dot: string_of_labels:('labels -> string) -> string_of_lits:('lits -> string) -> (_, 'labels, 'lits) t -> Dot.graph
end

module Make0(State: STATE)(States: STATES with type elt = State.t)(G: Graph.S with type vertex = State.t):
  S0 with type state = State.t and type states = States.t and type ('labels, 'lits) graph = ('labels, 'lits) G.t = struct

  module Start = struct
    module Via = struct
      type t =
        | Single of State.t
        | Multiple of States.t
      [@@deriving ord, eq]
    end

    type single = S
    type multiple = M

    type 'a t =
      | Single: State.t -> single t
      | Multiple: States.t -> multiple t

    let via: type a. a t -> Via.t = function
      | Single x -> Via.Single x
      | Multiple x -> Via.Multiple x

    let equal: type a. a t -> a t -> bool = fun x y ->
      Via.equal (via x) (via y)

    let compare: type a. a t -> a t -> int = fun x y ->
      Via.compare (via x) (via y)

  end

  type state = State.t
  type states = States.t
  type ('labels, 'lits) graph = ('labels, 'lits) G.t
  type ('a, 'labels, 'lits) t = {
    start: 'a Start.t;
    final: states;
    graph: ('labels, 'lits) graph;
  }

  let start a =
    match a.start with
    | Single start -> start

  let start_multiple: type a. (a, _, _) t -> States.t = fun a ->
    match a.start with
    | Single start -> States.singleton start
    | Multiple start -> start

  let final a =
    a.final

  let is_final q a =
    States.mem q a.final

  let states a =
    G.vertices a.graph

  let state_mem q a =
    G.vertex_mem q a.graph

  let transitions a =
    G.labeled_edges a.graph

  let adjacency_list a =
    G.adjacency_list a.graph

  let labels q m =
    G.vertex_label q m.graph

  let lits q1 q2 m =
    G.edge_label q1 q2 m.graph


  let all_lits  m =
    G.edge_labels m.graph

  let adjacent q a =
    G.adjacent q a.graph

  let rev a =
    {
      start = Multiple a.final;
      final = start_multiple a;
      graph = G.transpose a.graph
    }

  let empty =
    {
      start = Multiple States.empty;
      final = States.empty;
      graph = G.empty;
    }

  let merge ~merge_labels ~merge_lits m1 m2 =
    {
      start = Start.Multiple (States.union (start_multiple m1) (start_multiple m2));
      final = States.union m1.final m2.final;
      graph = G.merge ~merge_vertex_label:merge_labels ~merge_edge_label:merge_lits m1.graph m2.graph;
    }

  let sum m1 m2 =
    {
      start = Start.Multiple (States.union (start_multiple m1) (start_multiple m2));
      final = States.union m1.final m2.final;
      graph = G.sum m1.graph m2.graph;
    }

  let states_labels m =
    G.labeled_vertices m.graph

  let map_states_labels f m =
    {m with graph = G.labeled_vertices_map f m.graph}

  let homomorphism f m =
    {m with graph = G.labeled_edges_map (fun _ _ ls -> f ls) m.graph}

  let unfold_step ~final f q x =
    let (is_final, vl, y) = f q x in
    if is_final then final := States.add q !final;
    (vl, y)

  let unfold ?merge f q start =
    let final = ref States.empty in
    let graph = G.unfold ?merge (unfold_step ~final f) q start in
    {
      start = Single q;
      final = !final;
      graph
    }

  let digraph seed f a =
    G.digraph seed f a.graph

  let dijkstra seed f a =
    G.dijkstra seed f a.graph

  let fold ~eq seed f a =
    G.fold ~eq seed f a.graph

  let to_dot ~string_of_labels ~string_of_lits a =
    let node q l last labels = Dot.(node (State.to_id q) ~attrs:[
        "label" => String l;
        "peripheries" => String (if last then "2" else "1");
        "xlabel" => String labels
      ])
    and edge q0 q1 l = Dot.(edge Directed (State.to_id q0) (State.to_id q1) ~attrs:[
        "label" => String l
      ])
    and start_edges =
      start_multiple a
      |> States.to_seq
      |> Seq.map (fun q0 ->
          Dot.(edge Directed (String "start") (State.to_id q0)))
    and start_node = Seq.return
        Dot.(node (String "start") ~attrs:[
            "style" => String "invis"
          ])
    in
    let nodes = Seq.map (fun (q, l) -> 
        (node q (Fmt.to_to_string State.pp q) (States.mem q a.final) (string_of_labels l)))
        (G.labeled_vertices a.graph) 
    in
    let edges = Seq.map (fun (q0, q1, ss) ->
        edge q0 q1 (string_of_lits ss))
        (G.labeled_edges a.graph) 
    in
    Dot.(Digraph, "g", List.of_seq @@ Seq.(nodes @ edges @ start_node @ start_edges))
end

module type LITS = sig
  type t
  val pp: t Fmt.t
  val equal: t -> t -> bool
  val compare: t -> t -> int
end

module type LABELS = sig
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val empty: t
  val union: t -> t -> t
end


module type FA = sig
  include S0 with type state = T.State.t and type states = T.States.t and type ('labels, 'lits) graph = ('labels, 'lits) T.State_graph.t

  val skeleton: (_, 'labels, 'lits) t -> ('labels, 'lits) G.t


  (*val is_final_multiple: T.States.t -> ('a, 'labels, 'lits) t -> bool
  val adjacent_multiple: T.States.t -> ('a, 'labels, 'lits) t -> (T.State.t * 'lits) Seq.t
  val 'labels_multiple: T.States.t -> ('a, 'labels, 'lits) t -> 'labels*)

  val goto: T.State.t -> ('lits -> bool) -> ('a, 'labels, 'lits) t -> T.States.t

  val link: ?merge:('lits -> 'lits -> 'lits) -> T.States.t -> T.States.t -> 'lits -> ('a, 'labels, 'lits) t -> ('a, 'labels, 'lits) t

  module Gen(Index: G.INDEX): sig
    val unfold_multiple: supply:(T.State.t Supply.t) -> ?merge:('lits -> 'lits -> 'lits) -> (T.State.t -> Index.elt -> bool * 'labels * ('lits * Index.elt) Seq.t) -> Index.elt list -> (Start.multiple, 'labels, 'lits) t

    val unfold: supply:(T.State.t Supply.t) -> ?merge:('lits -> 'lits -> 'lits) -> (T.State.t -> Index.elt -> bool * 'labels * ('lits * Index.elt) Seq.t) -> Index.elt -> (Start.single, 'labels, 'lits) t
  end
end

module Make: FA = struct
  include Make0(T.State)(T.States)(T.State_graph)

  let skeleton m = m.graph

  let link ?(merge = Fun.const) f t ls m =
    {
      m with
      graph = G.link_with merge (T.States.to_seq f) (T.States.to_seq t) ls m.graph;
    }

  let goto from f m =
    adjacent from m
    |> Seq.filter_map (fun (to_, ls) -> if f ls then Some to_ else None)
    |> T.States.of_seq

  module Gen(Index: G.INDEX) = struct
    module Graph_gen = G.Gen(Index)

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
end
