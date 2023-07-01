(*
SPPF is a generalization of a graph, where each node contains multiple sets of children, which are ordered.
This seems like a general data-structure, is it known outside of parsing? 

The CFG are isomorphic to SPPF.
*)
open! Prelude

module type NODE = sig
  type t

  val equal: t -> t -> bool
  val pp: t Fmt.t
  val to_id: t -> Dot.id
end

module type NODE_MAP = sig
  type elt
  type  _ t

  val compare: ('a -> 'a -> int) ->  'a t ->  'a t -> int
  val equal: ('a -> 'a -> bool) ->  'a t ->  'a t -> bool
  val cardinal:  'a t -> int

  val find: elt ->  'a t -> 'a

  val empty:  'a t
  val singleton: elt -> 'a ->  'a t
  val add: elt -> 'a ->  'a t ->  'a t
  val to_seq:  'a t -> (elt * 'a) Seq.t

  (*val is_empty:  'a t -> bool
    val remove: elt ->  'a t ->  'a t
    val modify: elt -> ('b -> 'b) ->  'a t ->  'a t

    val mem: elt ->  'a t -> bool

    val map: (elt -> 'b -> 'c) ->  'a t -> ('a, 'c) t

    val of_seq: (elt * 'b) Seq.t ->  'a t

    val domain_disjoint:  'a t ->  'a t -> bool
    val union: (elt -> 'b -> 'b -> 'b) ->  'a t ->  'a t ->  'a t*)
end

module type S = sig
  type node
  type 'nl adjacents = ('nl * node list) list
  type 'nl t

  val adjacent: node -> 'nl t -> node list list
  val empty: _ t
  val add: node -> 'nl t -> 'nl t
  val pack: node -> 'nl -> node list -> 'nl t -> 'nl t
  val to_dot: 'nl t -> Dot.graph
end

module Make(Node: NODE)(Node_map: NODE_MAP with type elt = Node.t): S with type node = Node.t = struct
  type node = Node.t
  type 'nl adjacents = ('nl * Node.t list) list
  type 'nl ctx = 'nl adjacents
  type 'nl t = 'nl ctx Node_map.t

  let adjacent n t =
    let adjs = Node_map.find n t in
    List.map snd adjs

  let empty = Node_map.empty

  let add n t =
    Node_map.add n [] t

  let pack n nl adj t =
    let adjs = Node_map.find n t in
    if List.exists (fun (_, adj') -> List.equal Node.equal adj adj') adjs
    then t
    else Node_map.add n ((nl, adj) :: adjs) t

  let to_dot f =
    let ctxs = Node_map.to_seq f in

    let nodes = Seq.map (fun (n, adjs) ->
        let ports = List.mapi (fun i _ -> Dot.RecordPortString (Int.to_string i, "")) adjs in
        Dot.(node (Node.to_id n) ~attrs:[
            "shape" => Id "record";
            "label" => String (string_of_record [Record [RecordString (Fmt.str "%a" Node.pp n); Record ports]])
          ]))
        ctxs
    in
    let edges = Seq.flat_map (fun (n, adjs) -> 
        Seq.concat @@
        Seq.mapi (fun i (_, adj) ->
            Seq.map (fun n' -> Dot.edge Directed (Port (Node.to_id n, Int.to_string i)) (Node.to_id n')) (List.to_seq adj))
          (List.to_seq adjs))
        ctxs
    in
    Dot.((Digraph, "g", List.of_seq Seq.(nodes @ edges)))
end
