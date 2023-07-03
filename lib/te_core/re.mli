open Te_bot

module type OP = sig
  type (+_, +_) t
  val pp: 'vs Fmt.t -> 'ls Fmt.t -> ('vs, 'ls) t Fmt.t

  val nothing: _ t
  val null: _ t
  val any: _ t
  val comp_nothing: _ t
  val comp_null: _ t
  val comp_any: _ t
  val lits: 'ls -> ('vs, 'ls) t
  val concat: ('vs, 'ls) t -> ('vs, 'ls) t -> ('vs, 'ls) t
  val union: ('vs, 'ls) t -> ('vs, 'ls) t -> ('vs, 'ls) t
  val repeat: ('vs, 'ls) t -> int -> ('vs, 'ls) t
  val star: ('vs, 'ls) t -> ('vs, 'ls) t
  val comp: ('vs, 'ls) t -> ('vs, 'ls) t
end

module Abstract: sig
  type (+_, +_) t

  val equal: ('vs -> 'vs -> bool) -> ('ls -> 'ls -> bool) -> ('vs, 'ls) t -> ('vs, 'ls) t -> bool
  val compare: ('vs -> 'vs -> int) -> ('ls -> 'ls -> int) -> ('vs, 'ls) t -> ('vs, 'ls) t -> int

  val is_nullable: _ t -> bool
  val is_nothing: _ t -> bool
  val is_nullable': ('ls -> bool) -> ('vs, 'ls) t -> bool
  val reverse: ('vs, 'ls) t -> ('vs, 'ls) t

  include OP with type ('vs, 'ls) t := ('vs, 'ls) t
end

module type VARS = sig
  type t
  val pp: t Fmt.t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val subset: t -> t -> bool

  val empty: t
  val is_empty: t -> bool
  val inter: t -> t -> t
  val diff: t -> t -> t
  val union: t -> t -> t
end

module type LITS = sig
  type t
  type vars
  val pp: t Fmt.t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val subset: t -> t -> bool
  val to_vars: t -> vars
  val of_vars: vars -> t
end

module type CONCRETE = sig
  type vars
  type lits
  type t = (vars, lits) Abstract.t

  val pp: t Fmt.t
  val equal: t -> t -> bool
  val compare: t -> t -> int

  val derivative: lits -> t -> t
  val derivative': vars -> (vars -> t) -> (lits -> bool) -> lits -> t -> t

  val first': (t -> bool) -> t -> lits Seq.t
  val first: t -> lits Seq.t
  val free: t -> vars
  val simplify: t -> t

  val to_seq: (lits -> 'a Seq.t) -> t -> 'a Seq.t Seq.t
end

module Concrete(Vars: VARS)(Lits: LITS with type vars = Vars.t): CONCRETE with type lits = Lits.t and type vars = Vars.t

module Porcelan(C: OP): sig
  include OP with type ('vs, 'ls) t = ('vs, 'ls) C.t

  val comp_lits: 'ls -> ('vs, 'ls) t
  val comp_concat: ('vs, 'ls) t -> ('vs, 'ls) t -> ('vs, 'ls) t
  val comp_union: ('vs, 'ls) t -> ('vs, 'ls) t -> ('vs, 'ls) t
  val inter: ('vs, 'ls) t -> ('vs, 'ls) t -> ('vs, 'ls) t
  val comp_repeat: ('vs, 'ls) t -> int -> ('vs, 'ls) t
  val comp_star: ('vs, 'ls) t -> ('vs, 'ls) t
  val opt: ('vs, 'ls) t -> ('vs, 'ls) t
  val comp_opt: ('vs, 'ls) t -> ('vs, 'ls) t
  val force: ('vs, 'ls) t -> ('vs, 'ls) t
  val diff: ('vs, 'ls) t -> ('vs, 'ls) t -> ('vs, 'ls) t
  val comp_diff: ('vs, 'ls) t -> ('vs, 'ls) t -> ('vs, 'ls) t
  val wildcard: ('vs, 'ls) t
  val interval: ('vs, 'ls) t -> int -> int -> ('vs, 'ls) t
  val plus: ('vs, 'ls) t -> ('vs, 'ls) t
  val not: ('vs, 'ls) t -> ('vs, 'ls) t
  val universe: ('vs, 'ls) t
  val nonnull: ('vs, 'ls) t
  val ( * ): ('vs, 'ls) t -> ('vs, 'ls) t -> ('vs, 'ls) t
  val (+): ('vs, 'ls) t -> ('vs, 'ls) t -> ('vs, 'ls) t
  val (-): ('vs, 'ls) t -> ('vs, 'ls) t -> ('vs, 'ls) t
  val (&): ('vs, 'ls) t -> ('vs, 'ls) t -> ('vs, 'ls) t
  val (|..): ('vs, 'ls) t -> int -> ('vs, 'ls) t
  val (|...): ('vs, 'ls) t -> (int * int) -> ('vs, 'ls) t
end

module Kleene(Vars: VARS)(Lits: LITS with type vars = Vars.t)(G: Graph.S): sig
  open Concrete(Vars)(Lits) 
  val solve: ('a, t) G.t -> ('a, t) G.t
  val rev_solve: ('a, t) G.t -> ('a, t) G.t
end
