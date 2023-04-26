open Te_bot

module Code: sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val to_id: t -> Dot.id
  val pp: t Fmt.t
  val to_string: t -> string

  val of_int: int -> t
end

module Codes: sig
  type t
  type elt = Code.t
  include Comp_set.S with type elt := elt and type t := t

  val pp: t Fmt.t
  val of_string: string -> t
end

module State: sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val to_id: t -> Dot.id
  val pp: t Fmt.t
  val supply: t Supply.t
end
module States: sig
  type t
  type elt = State.t

  include Set.CORE with type elt := elt and type t := t
  include Set.BINARY with type elt := elt and type t := t
  include Set.SEQUENTIAL with type elt := elt and type t := t
end
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
  type pre
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val pp: t Fmt.t
  val supply: pre Supply.t

  val make: supply:(pre Supply.t) -> (string, 'a) Vector.t -> (t, 'a) Vector.t 
end

module Vars: sig
  include Set.CORE with type elt = Var.t
  include Set.BINARY with type elt := elt and type t := t
  val subset: t -> t -> bool
  val pp: t Fmt.t
end
module Var_to: sig
  include Map.CORE with type elt = Var.t
  val update: elt -> ('a option -> 'a option) -> 'a t -> 'a t
  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
end
