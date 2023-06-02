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
  val pp: t Fmt.t
  val disjoint: t -> t -> bool
  val cardinal: t -> int
  val choose: t -> elt option
end
module State_to: sig
  include Map.CORE with type elt = State.t
  val pp: 'a Fmt.t -> 'a t Fmt.t
  val subset: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end
module State_graph: Graph.S with type vertex = State.t

module State_pair: sig
  type t = State.t * State.t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val pp: t Fmt.t
end

module State_pairs: sig
  type t
  type elt = State_pair.t
  include Set.CORE with type elt := elt and type t := t
  include Set.BINARY with type elt := elt and type t := t
  include Set.SEQUENTIAL with type elt := elt and type t := t
  val pp: t Fmt.t
end

module State_pair_to: Map.CORE with type elt = State_pair.t
module State_pair_graph: Graph.S with type vertex = State_pair.t

module type INDEX_MAP = sig
  type elt
  type 'a t

  val find: elt -> 'b t -> 'b
  val empty: _ t
  val add: elt -> 'b -> 'b t -> 'b t
end
module State_index(Map: INDEX_MAP): State_graph.INDEX with type elt = Map.elt

module Vertex: sig
  type t
  val make: State.t -> int -> t
  val state: t -> State.t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val to_id: t -> Dot.id
  val pp: t Fmt.t
end

module Vertices: sig
  type t
  type elt = Vertex.t
  include Set.CORE with type elt := elt and type t := t
  include Set.BINARY with type elt := elt and type t := t
  include Set.SEQUENTIAL with type elt := elt and type t := t
  val pp: t Fmt.t
end
module Vertex_to: Map.CORE with type elt = Vertex.t
module Vertex_graph: Graph.S with type vertex = Vertex.t

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
  type t
  type elt = Var.t
  include Set.CORE with type elt := elt and type t := t
  include Set.BINARY with type elt := elt and type t := t
  include Set.SEQUENTIAL with type elt := elt and type t := t
  val pp: t Fmt.t
  val subset: t -> t -> bool
  val exists: (elt -> bool) -> t -> bool
  val choose: t -> elt option
end
module Var_to: sig
  include Map.CORE with type elt = Var.t
  val update: elt -> ('a option -> 'a option) -> 'a t -> 'a t
  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
end

module Labeled_var: sig
  type t = Var.t * Var.t

  val equal: t -> t -> bool
  val compare: t -> t -> int
  val var: t -> Var.t
  val node: t -> Var.t
  val pp: t Fmt.t
end

module Labeled_vars: sig
  type t
  type elt = Labeled_var.t
  include Set.CORE with type elt := elt and type t := t
  include Set.BINARY with type elt := elt and type t := t
  include Set.SEQUENTIAL with type elt := elt and type t := t
  val pp: t Fmt.t
  val disjoint: t -> t -> bool
  val subset: t -> t -> bool
end

module Labeled_var_to: sig
  include Map.CORE with type elt = Labeled_var.t
  val pp: 'a Fmt.t -> 'a t Fmt.t
  val update: elt -> ('a option -> 'a option) -> 'a t -> 'a t
  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val of_seq: (elt * 'a) Seq.t -> 'a t
end
