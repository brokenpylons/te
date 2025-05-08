open Te_bot

val eof_string: string
val delegate_string: string

val explode: string -> int list

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
  val of_int: int -> t
  val of_int_list: int list -> t
  val of_string: string -> t
end

module Code_to: sig
  include Map.CORE with type elt = Code.t

  val pp: 'a Fmt.t -> 'a t Fmt.t
  val update: elt -> ('a option -> 'a option) -> 'a t -> 'a t
  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter: (elt -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val diff: (elt -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val filter: (elt -> 'a -> bool) -> 'a t -> 'a t
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
  val iter: (elt -> unit) -> t -> unit
end
module State_to: sig
  include Map.CORE with type elt = State.t
  val pp: 'a Fmt.t -> 'a t Fmt.t
  val subset: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val update: elt -> ('a option -> 'a option) -> 'a t -> 'a t
  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter: (elt -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val diff: (elt -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val filter: (elt -> 'a -> bool) -> 'a t -> 'a t
  val of_seq: (elt * 'a) Seq.t -> 'a t
  val to_seq: 'a t -> (elt * 'a) Seq.t
  val to_list: 'a t -> (elt * 'a) list
  val cardinal: 'a t -> int
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
  val iter: (elt -> unit) -> t -> unit
end

module States_to: sig
  include Map.CORE with type elt = States.t
  val pp: 'a Fmt.t -> 'a t Fmt.t
  val subset: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val update: elt -> ('a option -> 'a option) -> 'a t -> 'a t
  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter: (elt -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val diff: (elt -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
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
  val cardinal: t -> int
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
  val is_empty: t -> bool
end

module State_pair_partial: sig
  type t = State_pair.t option
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val pp: t Fmt.t

  val union: t -> t -> t
  val empty: t
  val is_empty: t -> bool
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

module Preserve_state_index: State_graph.INDEX with type elt = States.t

module Vertex: sig
  type t
  val make: State.t -> int -> t
  val states: t -> State.t
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
  val inter: (elt -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val diff: (elt -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
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
  val dummy: t

  val make: supply:(pre Supply.t) -> (string, 'a) Vector.t -> (t, 'a) Vector.t 
  val make': pre Supply.t -> string -> (t * pre Supply.t)
  val synthetic: pre Supply.t -> (t * pre Supply.t)
end

module Vars: sig
  type t
  type elt = Var.t
  include Set.CORE with type elt := elt and type t := t
  include Set.BINARY with type elt := elt and type t := t
  include Set.SEQUENTIAL with type elt := elt and type t := t
  val pp: t Fmt.t
  val disjoint: t -> t -> bool
  val subset: t -> t -> bool
  val exists: (elt -> bool) -> t -> bool
  val choose: t -> elt option
  val iter: (elt -> unit) -> t -> unit
end
module Var_to: sig
  include Map.CORE with type elt = Var.t
  val pp: 'a Fmt.t -> 'a t Fmt.t
  val update: elt -> ('a option -> 'a option) -> 'a t -> 'a t
  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter: (elt -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val diff: (elt -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
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
  val iter: (elt -> unit) -> t -> unit
end

module Labeled_var_to: sig
  include Map.CORE with type elt = Labeled_var.t
  val pp: 'a Fmt.t -> 'a t Fmt.t
  val map: (elt -> 'a -> 'b) -> 'a t -> 'b t
  val update: elt -> ('a option -> 'a option) -> 'a t -> 'a t
  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter: (elt -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val diff: (elt -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val filter: (elt -> 'a -> bool) -> 'a t -> 'a t
  val of_seq: (elt * 'a) Seq.t -> 'a t
  val to_seq: 'a t -> (elt * 'a) Seq.t
end

module Reduction: sig
  module Strategy: sig
    type t = Null | Fixed of int | Scan of State_pair.t
    val compare: t -> t -> int
    val equal: t -> t -> bool
  end
  module Reminder: sig
    type t = Complete | Lists of Var.t list list | Gen of State_pair.t
    val compare: t -> t -> int
    val equal: t -> t -> bool
  end
  type t = private {output: Labeled_var.t; strategy: Strategy.t; reminder: Reminder.t}
  val make: Labeled_var.t -> Strategy.t -> Reminder.t -> t

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

module Symbol: sig
  type t = Delegate | Eof | Code of Code.t | Var of Var.t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val pp: t Fmt.t
end

module Symbol_to: sig
  include Map.CORE with type elt = Symbol.t
  val pp: 'a Fmt.t -> 'a t Fmt.t
  val update: elt -> ('a option -> 'a option) -> 'a t -> 'a t
  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter: (elt -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val diff: (elt -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val filter: (elt -> 'a -> bool) -> 'a t -> 'a t
end

module Symbols: sig
  type t

  val pp: t Fmt.t

  type elt = Symbol.t
  val of_vars: Vars.t -> t
  val of_codes: Codes.t -> t

  val to_vars: t -> Vars.t
  val to_codes: t -> Codes.t

  val is_eof: t -> bool
  val is_delegate: t -> bool

  val eof: t
  val delegate: t
  val add: Symbol.t -> t -> t
  val singleton: Symbol.t -> t
  val empty: t

  val subset: t -> t -> bool
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
end

module type SYMBOL_MULTIMAP_VALUES = sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool

  val empty: t
  val union: t -> t -> t
end

module Symbols_multimap(Values: SYMBOL_MULTIMAP_VALUES): sig
  type key = Symbols.t
  type values = Values.t
  type t

  val equal: t -> t -> bool
  val compare: t -> t -> int
  val pp: values Fmt.t -> t Fmt.t

  val empty: t
  val add_multiple: key -> values -> t -> t
  val singleton_multiple: key -> values -> t
  val union: t -> t -> t
  val (<|>): t -> t -> t
  val find_multiple: key -> t -> values
  val find_multiple_or_empty: key -> t -> values
  val of_seq_multiple: (key * values) Seq.t -> t
end

module Node: sig
  type t
  val make: Symbol.t -> int -> int -> t
  val symbol: t -> Symbol.t

  val compare: t -> t -> int
  val equal: t -> t -> bool
  val to_id: t -> Dot.id
  val pp: t Fmt.t
end
module Nodes: sig
  type t
  type elt = Node.t
  include Set.CORE with type elt := elt and type t := t
  include Set.BINARY with type elt := elt and type t := t
  include Set.SEQUENTIAL with type elt := elt and type t := t
  val pp: t Fmt.t
  val disjoint: t -> t -> bool
  val subset: t -> t -> bool
  val exists: (elt -> bool) -> t -> bool
  val choose: t -> elt option
  val iter: (elt -> unit) -> t -> unit
end
module Node_packed_forest: Packed_forest.S with type node = Node.t and type labels = Vars.t

module Actions: sig
  type t = private
    {
      accept: bool;
      shift: bool;
      load: bool;
      orders: Vars.t;
      matches: Labeled_vars.t;
      predictions: Vars.t;
      null: Reductions.t;
      reduce: Reductions.t;
    }
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val pp: t Fmt.t

  val union: t -> t -> t
  val empty: t
  val is_empty: t -> bool
  val accept: t
  val shift: t
  val load: t
  val orders: Vars.t -> t
  val matches: Labeled_vars.t -> t
  val predictions: Vars.t -> t
  val null: Reductions.t -> t
  val reduce: Reductions.t -> t
end
