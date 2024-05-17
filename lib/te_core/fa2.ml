open Te_bot
open! Prelude
module T = Types

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

module type S = sig
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

  type ('a, 'labels, 'lits) t

  val start: (Start.single, _, _) t -> state
  val start_multiple: _ t -> states
  val labels: state -> ('a, 'labels, 'lits) t -> 'labels
  val transitions: (_, _, 'lits) t -> (state * state * 'lits) Seq.t
  val adjacent: state -> (_, _, 'lits) t -> (state * 'lits) Seq.t
  val adjacent_multiple: states -> (_, _, 'lits) t -> (state * 'lits) Seq.t
  val goto: state -> ('lits -> bool) -> ('a, 'labels, 'lits) t -> states

  val unfold:
    ?merge: ('lits -> 'lits -> 'lits) ->
    (state -> 'seed -> 'labels * ('lits * state * 'seed) Seq.t) -> state -> 'seed -> (Start.single, 'labels, 'lits) t

  val digraph: (state -> 'labels -> 'seed) ->
    ('seed -> 'labels -> ('lits * (unit -> 'seed)) Seq.t -> 'seed) ->
    (Start.single, 'labels, 'lits) t ->
    (state -> 'seed)

  val dijkstra: (state -> 'labels -> 'seed) ->
    ('seed -> 'labels -> ('lits * (unit -> 'seed)) Seq.t -> 'seed) ->
    (Start.single, 'labels, 'lits) t ->
    (state -> 'seed)


  val tail: state -> (Start.single, 'labels, 'lits) t -> (Start.single, 'labels, 'lits) t

  module type INDEX = sig
    type t
    type elt
    val update: elt -> t -> [> `New of state * elt * t | `Old of state]
    val make: state Supply.t -> t
  end

  module Gen(Index: INDEX): sig
    val unfold_multiple: supply:(state Supply.t) -> ?merge:('lits -> 'lits -> 'lits) -> (state -> Index.elt -> 'labels * ('lits * Index.elt) Seq.t) -> Index.elt list -> (Start.multiple, 'labels, 'lits) t
    val unfold: supply:(state Supply.t) -> ?merge:('lits -> 'lits -> 'lits) -> (state -> Index.elt ->  'labels * ('lits * Index.elt) Seq.t) -> Index.elt -> (Start.single, 'labels, 'lits) t
  end

  val to_dot: string_of_labels:('labels -> string) -> string_of_lits:('lits -> string) -> (_, 'labels, 'lits) t -> Dot.graph
end

module Make(State: STATE)(States: STATES with type elt = State.t)(G: Graph.S with type vertex = State.t): S with type state = State.t and type states = States.t = struct
  type state = State.t
  type states = States.t

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

  type ('a, 'labels, 'lits) t = {
    start: 'a Start.t;
    graph: ('labels, 'lits) G.t;
  }

  let start a =
    match a.start with
    | Single start -> start

  let start_multiple: type a. (a, _, _) t -> States.t = fun a ->
    match a.start with
    | Single start -> States.singleton start
    | Multiple start -> start

  let transitions a =
    G.labeled_edges a.graph

  let adjacent q a =
    G.adjacent q a.graph

  let labels q a =
    G.vertex_label q a.graph

  let goto from f a =
    adjacent from a
    |> Seq.filter_map (fun (to_, ls) -> if f ls then Some to_ else None)
    |> States.of_seq

  let adjacent_multiple qs a =
    Seq.flat_map (Fun.flip adjacent a) @@ States.to_seq qs

  let unfold ?merge f q start =
    let graph = G.unfold ?merge f q start in
    {
      start = Single q;
      graph
    }

  let tail q a =
    {a with start = Single q}

  let digraph seed f a =
    G.digraph seed f a.graph

  let dijkstra seed f a =
    G.dijkstra seed f a.graph

  module type INDEX = sig
    type t
    type elt
    val update: elt -> t -> [> `New of State.t * elt * t | `Old of State.t]
    val make: State.t Supply.t -> t
  end

  module Gen(Index: INDEX) = struct
    module G_gen = G.Gen(Index)

    let unfold_multiple ~supply ?merge f start =
      let start, graph = G_gen.unfold_multiple ~supply ?merge f start in
      {
        start = Multiple (States.of_list start);
        graph;
      }

    let unfold ~supply ?merge f start =
      let start, graph = G_gen.unfold ~supply ?merge f start in
      {
        start = Single start;
        graph
      }
  end


  let to_dot ~string_of_labels ~string_of_lits a =
    let node q lbls = Dot.(node (State.to_id q) ~attrs:[
        "label" => String (Fmt.to_to_string State.pp q);
        "xlabel" => String lbls
      ])
    and edge s q syms = Dot.(edge Directed (State.to_id s) (State.to_id q) ~attrs:[
        "label" => String syms
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
    let nodes = Seq.map (fun (q, lbls) ->
        (node q (string_of_labels lbls)))
        (G.labeled_vertices a.graph)
    in
    let edges = Seq.map (fun (s, q, syms) ->
        edge s q (string_of_lits syms))
        (G.labeled_edges a.graph)
    in
    Dot.(Digraph, "g", List.of_seq @@ Seq.(nodes @ edges @ start_node @ start_edges))
end
