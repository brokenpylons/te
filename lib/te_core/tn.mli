open Te_bot
module T = Types

module Lits: sig
  type t
  include Re.LITS with type t := t
  include Refine.PARTITION with type t := t
  val is_return: t -> bool

  val eof: t
  val empty: t
  val null: t
  val call: t
  val return: T.Vars.t -> t
  val var: T.Var.t -> t
  val vars: T.Vars.t -> t
  val codes: T.Codes.t -> t
end
module Enhanced_lits: Re.LITS

module Labels: sig
  type t = {states: T.States.t; predictions: T.Labeled_vars.t; matches: T.Labeled_vars.t}

  val empty: t
  val union: t -> t -> t
  val pp: t Fmt.t
end

val refine: Lits.t Seq.t -> Lits.t Seq.t

module Rhs: Re.CONCRETE with type lits = Lits.t

module M: Fa.FA with type labels = Labels.t and type lits = Lits.t

module Production: sig
  type t
  val make: T.Labeled_var.t -> Rhs.t -> t
  val lhs: t -> T.Labeled_var.t
  val rhs: t -> Rhs.t
  val pp: t Fmt.t
end

module P: Fa.FA with type labels = T.State_pairs.t and type lits = Lits.t

(*
module Grammar: sig
  type t = private {start: T.Var.t; symbols: T.Vars.t; terminal: T.Vars.t; productions: Production.t list}
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val make: start:T.Var.t -> Production.t list -> t
end
   *)

val convert: supply:T.State.t Supply.t -> Production.t -> T.Labeled_var.t * M.Start.single M.t
val convert_multiple: supply:T.State.t Supply.t -> Production.t Seq.t -> (T.Labeled_var.t * M.Start.single M.t) Seq.t
val construct: supply:T.State.t Supply.t -> T.Var.t -> Production.t Seq.t -> M.Start.single M.t
val inter: M.Start.single M.t -> M.Start.single M.t -> (bool, Lits.t) T.State_pair_graph.t
val erase_call: M.Start.multiple M.t -> M.Start.multiple M.t
val erase_return: 'a M.t -> 'a M.t
val collapse: supply:T.State.t Supply.t -> _ M.t -> M.Start.single M.t
val subset: supply:T.State.t Supply.t -> _ M.t -> M.Start.single M.t
val minimize: supply:T.State.t Supply.t -> M.Start.single M.t -> M.Start.single M.t
val earley: supply:T.State.t Supply.t -> M.Start.multiple M.t -> M.Start.single M.t

