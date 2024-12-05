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

  val mem: elt ->  _ t -> bool
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
  type labels
  type adjacents = (labels * node list) list
  type t

  val adjacent: node -> t -> node list list
  val empty: t
  val add: node -> t -> t
  val mem: node -> t -> bool
  val pack: node -> labels -> node list -> t -> t
  val to_dot: t -> Dot.graph
end

module type LABELS = sig
  type t
  val pp: t Fmt.t

  val subset: t -> t -> bool
  val union: t -> t -> t
  val empty: t
end

module Make(Node: NODE)(Labels: LABELS)(Node_map: NODE_MAP with type elt = Node.t): S with type node = Node.t and type labels = Labels.t = struct
  type node = Node.t
  type labels = Labels.t
  type adjacents = (Labels.t * Node.t list) list
  type ctx = adjacents
  type t = ctx Node_map.t

  let adjacent n t =
    let adjs = Node_map.find n t in
    List.map snd adjs

  let empty = Node_map.empty

  let mem n t = Node_map.mem n t

  let add n t =
    Node_map.add n [] t

  let pack n nl adj t =
    let adjs = Node_map.find n t in
    if List.exists (fun (_, adj') -> List.equal Node.equal adj adj') adjs
    then Node_map.add n (List.map (fun (nl', adj') -> 
        if List.equal Node.equal adj adj'
        then  (Labels.union nl nl', adj')
        else (nl', adj')) adjs) t
    else Node_map.add n ((nl, adj) :: adjs) t

  let to_dot f =
    let ctxs = Node_map.to_seq f in

    let nodes = Seq.map (fun (n, adjs) ->
        let ports = List.mapi (fun i (lbls, _) -> Dot.(RecordPortString (Int.to_string i, escape_string @@ Fmt.str "%a" Labels.pp lbls))) adjs in
        Dot.(node (Node.to_id n) ~attrs:[
            "shape" => Id "record";
            "label" => String (string_of_record [Record [RecordString (escape_string @@ Fmt.str "%a" Node.pp n); Record ports]])
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
