open Te_bot

module Code: sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val to_id: t -> Dot.id
  val pp: t Fmt.t
end
module Codes: sig
  include Set.CORE with type elt = Code.t
  val subset: t -> t -> bool
  val pp: t Fmt.t
end

module State: sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val to_id: t -> Dot.id
  val pp: t Fmt.t
end
module States: Set.CORE with type elt = State.t
module State_to: Map.CORE with type elt = State.t
module State_graph: Graph.S with type vertex = State.t

module type INDEX_MAP = sig
  type elt
  type 'a t

  val find: elt -> 'b t -> 'b
  val empty: _ t
  val add: elt -> 'b -> 'b t -> 'b t
end
module State_index(Map: INDEX_MAP): State_graph.INDEX with type elt = Map.elt

module Var: sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val pp: t Fmt.t

  val make: ?label:string -> int -> t
end

module Vars: sig
  include Set.CORE with type elt = Var.t
  val subset: t -> t -> bool
  val pp: t Fmt.t
end
module Var_to: Map.CORE with type elt = Var.t
