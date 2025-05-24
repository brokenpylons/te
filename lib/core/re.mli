open Te_bot

module Abstract: sig
  type +_ t

  val equal: ('ls -> 'ls -> bool) -> 'ls t -> 'ls t -> bool
  val compare: ('ls -> 'ls -> int) -> 'ls t -> 'ls t -> int

  exception Undefined
  val to_seq: cmp:('b -> 'b -> int) -> ?any:'b Seq.t -> ('a -> 'b Seq.t) -> 'a t -> 'b Seq.t Seq.t

  val map: ('a -> 'b) -> 'a t -> 'b t
  val flat_map: ('a -> 'b t) -> 'a t -> 'b t
  val unextend: 'supply -> ('supply -> 'a * 'supply) -> ('a -> 'ls)  -> 'ls t -> 'ls t * ('a * 'ls t) list
  val enumerate: 'a t -> 'a Seq.t
  val is_extended1: _ t -> bool
  val is_extended2: _ t -> bool

  val is_nullable: _ t -> bool
  val is_nothing: _ t -> bool
  val is_infinite: _ t -> bool
  val is_nullable': ('ls -> bool) -> 'ls t -> bool
  val reverse: 'ls t -> 'ls t

  val nothing: _ t
  val null: _ t
  val any: _ t
  val comp_nothing: _ t
  val comp_null: _ t
  val comp_any: _ t
  val lits: 'ls -> 'ls t
  val concat: 'ls t -> 'ls t -> 'ls t
  val union: 'ls t -> 'ls t -> 'ls t
  val repeat: 'ls t -> int -> 'ls t
  val star: 'ls t -> 'ls t
  val comp: 'ls t -> 'ls t

  val comp_lits: 'ls -> 'ls t
  val comp_concat: 'ls t -> 'ls t -> 'ls t
  val comp_union: 'ls t -> 'ls t -> 'ls t
  val inter: 'ls t -> 'ls t -> 'ls t
  val comp_repeat: 'ls t -> int -> 'ls t
  val comp_star: 'ls t -> 'ls t
  val opt: 'ls t -> 'ls t
  val comp_opt: 'ls t -> 'ls t
  val force: 'ls t -> 'ls t
  val diff: 'ls t -> 'ls t -> 'ls t
  val comp_diff: 'ls t -> 'ls t -> 'ls t
  val wildcard: 'ls t
  val interval: 'ls t -> int -> int -> 'ls t
  val plus: 'ls t -> 'ls t
  val not: 'ls t -> 'ls t
  val universe: 'ls t
  val nonnull: 'ls t
  val ( * ): 'ls t -> 'ls t -> 'ls t
  val (+): 'ls t -> 'ls t -> 'ls t
  val (-): 'ls t -> 'ls t -> 'ls t
  val (~-): 'ls t -> 'ls t
  val (&): 'ls t -> 'ls t -> 'ls t
  val (|..): 'ls t -> int -> 'ls t
  val (|...): 'ls t -> (int * int) -> 'ls t
end

module type LITS = sig
  type t
  val pp: t Fmt.t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val subset: t -> t -> bool
end

module type CONCRETE = sig
  type lits
  type t = lits Abstract.t

  val pp: t Fmt.t
  val equal: t -> t -> bool
  val compare: t -> t -> int

  val derivative: lits -> t -> t

  val first': (t -> bool) -> t -> lits Seq.t
  val first: t -> lits Seq.t
  val simplify: t -> t
end

module Concrete(Lits: LITS): CONCRETE with type lits = Lits.t

module Kleene(Lits: LITS)(G: Graph.S): sig
  open Concrete(Lits)
  val solve: ('a, t) G.t -> ('a, t) G.t
  val rev_solve: ('a, t) G.t -> ('a, t) G.t
end
