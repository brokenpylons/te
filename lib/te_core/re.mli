module type OP = sig
  type +_ t
  val pp: 'a Fmt.t -> 'a t Fmt.t

  val nothing: _ t
  val null: _ t
  val any: _ t
  val comp_nothing: _ t
  val comp_null: _ t
  val comp_any: _ t
  val lits: 'a -> 'a t
  val concat: 'a t -> 'a t -> 'a t
  val union: 'a t -> 'a t -> 'a t
  val repeat: 'a t -> int -> 'a t
  val star: 'a t -> 'a t
  val comp: 'a t -> 'a t
end

module Abstract: sig
  type +_ t

  val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int

  val is_nullable: _ t -> bool
  val is_nullable': ('a -> bool) -> 'a t -> bool
  val reverse: 'a t -> 'a t

  include OP with type 'a t := 'a t
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

  val is_nullable: t -> bool
  val is_nullable': (lits -> bool) -> t -> bool
  val is_nothing: t -> bool
  val derivative: lits -> t -> t
  val derivative': lits list -> t -> t

  val first: t -> lits Seq.t
  val occur: t -> lits Seq.t
  val simplify: t -> t
end

module Concrete(Lits: LITS): CONCRETE with type lits = Lits.t 

module Porcelan(C: OP): sig
  include OP with type 'a t = 'a C.t

  val comp_lits: 'a -> 'a t
  val comp_concat: 'a t -> 'a t -> 'a t
  val comp_union: 'a t -> 'a t -> 'a t
  val inter: 'a t -> 'a t -> 'a t
  val comp_repeat: 'a t -> int -> 'a t
  val comp_star: 'a t -> 'a t
  val opt: 'a t -> 'a t
  val comp_opt: 'a t -> 'a t
  val force: 'a t -> 'a t
  val diff: 'a t -> 'a t -> 'a t
  val comp_diff: 'a t -> 'a t -> 'a t
  val wildcard: 'a t
  val interval: 'a t -> int -> int -> 'a t
  val plus: 'a t -> 'a t
  val not: 'a t -> 'a t
  val universe: 'a t
  val nonnull: 'a t
  val ( * ): 'a t -> 'a t -> 'a t
  val (+): 'a t -> 'a t -> 'a t
  val (-): 'a t -> 'a t -> 'a t
  val (&): 'a t -> 'a t -> 'a t
  val (|..): 'a t -> int -> 'a t
  val (|...): 'a t -> (int * int) -> 'a t
end
