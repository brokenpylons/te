open Te_bot
module T = Types

module Lits: sig
  type t = {call: bool; return: bool; null: bool; vars: T.Vars.t; codes: T.Codes.t}
  include Re.LITS with type t := t
  val empty: t
  val null: t
  val call: t
  val return: t
  val var: T.Var.t -> t
  val codes: T.Codes.t -> t

  val is_empty: t -> bool
  val comp: t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
end
val refine: Lits.t Seq.t -> Lits.t Seq.t

module M: Fa.FA with type lits = Lits.t
module R: Re.CONCRETE with type lits = Lits.t

module Production: sig
  type t = {lhs: T.Var.t; rhs: R.t}
  val make: T.Var.t -> R.t -> t
  val equal: t -> t -> bool
  val compare: t -> t -> int
end

val convert: supply:T.State.t Supply.t -> Production.t -> M.Start.single M.t
val expand: supply:T.State.t Supply.t -> T.Var.t -> Production.t list -> M.Start.multiple M.t
