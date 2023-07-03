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
  type t = int
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val to_id: t -> Dot.id
  val pp: t Fmt.t
  val supply: t Supply.t
  val fresh_supply: unit -> t Supply.t
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
  val to_id: t -> Dot.id
end
module State_to: sig
  include Map.CORE with type elt = State.t
  val pp: 'a Fmt.t -> 'a t Fmt.t
  val subset: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val update: elt -> ('a option -> 'a option) -> 'a t -> 'a t
  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val diff: 'a t -> 'a t -> 'a t
  val filter: (elt -> 'a -> bool) -> 'a t -> 'a t
  val of_seq: (elt * 'a) Seq.t -> 'a t
  val to_seq: 'a t -> (elt * 'a) Seq.t
end
module State_graph: Graph.S with type vertex = State.t

module Statess: sig
  type t
  type elt = States.t
  include Set.CORE with type elt := elt and type t := t
  include Set.BINARY with type elt := elt and type t := t
  include Set.SEQUENTIAL with type elt := elt and type t := t
  val pp: t Fmt.t
  val disjoint: t -> t -> bool
  val cardinal: t -> int
  val choose: t -> elt option
end

module States_to: sig
  include Map.CORE with type elt = States.t
  val pp: 'a Fmt.t -> 'a t Fmt.t
  val subset: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val update: elt -> ('a option -> 'a option) -> 'a t -> 'a t
  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val diff: 'a t -> 'a t -> 'a t
  val filter: (elt -> 'a -> bool) -> 'a t -> 'a t
  val of_seq: (elt * 'a) Seq.t -> 'a t
end
module States_graph: Graph.S with type vertex = States.t


module State_pair: sig
  type t = State.t * State.t
  val to_id: t -> Dot.id
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

module State_pair_to: sig
  include Map.CORE with type elt = State_pair.t
  val pp: 'a Fmt.t -> 'a t Fmt.t
  val of_seq: (elt * 'a) Seq.t -> 'a t
end
module State_pair_graph: Graph.S with type vertex = State_pair.t

module State_partial: sig
  type t = State.t option
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val pp: t Fmt.t

  val union: t -> t -> t
  val empty: t
end

module State_pair_partial: sig
  type t = State_pair.t option
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val pp: t Fmt.t

  val union: t -> t -> t
  val empty: t
end

module States_partial: sig
  type t = States.t option
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val pp: t Fmt.t

  val union: t -> t -> t
  val empty: t
end

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
  val make: States.t -> int -> t
  val states: t -> States.t
  val position: t -> int
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
  val iter: (elt -> unit) -> t -> unit
end
module Vertex_to: sig
  include Map.CORE with type elt = Vertex.t
  val pp: 'a Fmt.t -> 'a t Fmt.t
  val update: elt -> ('a option -> 'a option) -> 'a t -> 'a t
  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val diff: 'a t -> 'a t -> 'a t
  val filter: (elt -> 'a -> bool) -> 'a t -> 'a t
end
module Vertex_graph: Graph.S with type vertex = Vertex.t

module Edge: sig
  type t = Vertex.t * Vertex.t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val pp: t Fmt.t
end

module Edges: sig
  type t
  type elt = Edge.t
  include Set.CORE with type elt := elt and type t := t
  include Set.BINARY with type elt := elt and type t := t
  include Set.SEQUENTIAL with type elt := elt and type t := t
  val pp: t Fmt.t
end
module Edge_to: Map.CORE with type elt = Edge.t

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
  val pp: 'a Fmt.t -> 'a t Fmt.t
  val update: elt -> ('a option -> 'a option) -> 'a t -> 'a t
  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val diff: 'a t -> 'a t -> 'a t
  val filter: (elt -> 'a -> bool) -> 'a t -> 'a t
  val of_seq: (elt * 'a) Seq.t -> 'a t
end

module Labeled_var: sig
  type t = Var.t * Var.t

  val equal: t -> t -> bool
  val compare: t -> t -> int
  val var: t -> Var.t
  val label: t -> Var.t
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
  val map: (elt -> 'a -> 'b) -> 'a t -> 'b t
  val update: elt -> ('a option -> 'a option) -> 'a t -> 'a t
  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val diff: 'a t -> 'a t -> 'a t
  val filter: (elt -> 'a -> bool) -> 'a t -> 'a t
  val of_seq: (elt * 'a) Seq.t -> 'a t
  val to_seq: 'a t -> (elt * 'a) Seq.t
end

module Node: sig
  type t
  val make: Var.t -> int -> int -> t
  val var: t -> Var.t

  val compare: t -> t -> int
  val equal: t -> t -> bool
  val to_id: t -> Dot.id
  val pp: t Fmt.t
end
module Node_packed_forest: Packed_forest.S with type node = Node.t and type labels = Vars.t

module Reduction: sig
  module Strategy: sig
    type t = Null | Fixed of int | Scan of State_pair.t
    val compare: t -> t -> int
    val equal: t -> t -> bool
  end

  type t = private {output: Labeled_var.t; strategy: Strategy.t; reminder: Var.t list list}
  val make: Labeled_var.t -> Strategy.t -> Var.t list list -> t

  val compare: t -> t -> int
  val equal: t -> t -> bool
end
module Reductions: sig
  type t
  type elt = Reduction.t
  include Set.CORE with type elt := elt and type t := t
  include Set.BINARY with type elt := elt and type t := t
  include Set.SEQUENTIAL with type elt := elt and type t := t
  val pp: t Fmt.t
  val iter: (elt -> unit) -> t -> unit
  (*val disjoint: t -> t -> bool
  val subset: t -> t -> bool*)
end

