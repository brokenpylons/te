open Te_bot
open! Prelude
module T = Types

module type ELEMS = sig
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int
end

module Make(Elems: ELEMS) = struct
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

  type 'a t = {
    start: 'a Start.t;
    final: T.States.t;
    graph: (unit, Elems.t) T.State_graph.t;
  }

  let start_multiple: type a. a t -> T.States.t = fun m -> 
    match m.start with
    | Single start -> T.States.singleton start
    | Multiple start -> start

  module Gen(Index: T.State_graph.INDEX) = struct
    module Graph_gen = T.State_graph.Gen(Index)

    let unfold_step ~final f q x =
      let (is_final, vl, y) = f q x in
      if is_final then final := T.States.add q !final;
      (vl, y)

    let unfold_multiple ~supply f start =
      let final = ref T.States.empty in
      let start, graph = Graph_gen.unfold_multiple ~supply (unfold_step ~final f) start in
      {
        start = Multiple (T.States.of_list start);
        final = !final;
        graph;
      }

    let unfold ~supply f start =
      let final = ref T.States.empty in
      let start, graph = Graph_gen.unfold ~supply (unfold_step ~final f) start in
      {
        start = Single start;
        final = !final;
        graph
      }
  end

  let to_dot ~string_of_label ~string_of_elt a =
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
        (node q (Fmt.to_to_string T.State.pp q) (T.States.mem q a.final) (string_of_label l)))
        (T.State_graph.labeled_vertices a.graph) 
    in
    let edges = Seq.map (fun (q0, q1, ss) ->
        edge q0 q1 (string_of_elt ss))
        (T.State_graph.labeled_edges a.graph) 
    in
    Dot.(Digraph, "g", List.of_seq @@ Seq.(nodes @ edges @ start_node @ start_edges))
end
