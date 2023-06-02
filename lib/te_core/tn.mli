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
  val call: T.Vars.t -> t
  val return: T.Vars.t -> t
  val var: T.Var.t -> t
  val vars: T.Vars.t -> t
  val codes: T.Codes.t -> t
end
module Enhanced_lits: sig
  include Re.LITS with type t = Lits.t T.State_to.t
end

module Rhs: Re.CONCRETE with type lits = Lits.t

type items

module Labels: sig
  type t = {items: items; tail: Rhs.t; started: T.Labeled_vars.t; predictions: T.Labeled_vars.t; matches: T.Labeled_vars.t}

  val empty: t
  val union: t -> t -> t
  val pp: t Fmt.t
end

val refine: Lits.t Seq.t -> Lits.t Seq.t


module M: Fa.FA with type labels = Labels.t and type lits = Lits.t
module M': Fa.FA with type labels = Labels.t and type lits = Enhanced_lits.t

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

type to_start

val convert: supply:T.State.t Supply.t -> Production.t -> T.Labeled_var.t * M.Start.single M.t
val convert_multiple: supply:T.State.t Supply.t -> Production.t Seq.t -> (T.Labeled_var.t * M.Start.single M.t) Seq.t
val is_nullable: Production.t Seq.t -> T.Vars.t

(*val construct'': T.Var.t -> T.State.t -> ((T.Labeled_var.t * T.State.t) * M'.Start.single M'.t) Seq.t -> M'.Start.single M'.t*)

val construct: T.Var.t -> (T.Labeled_var.t * M.Start.single M.t) Seq.t -> M.Start.single M.t

(*val construct: supply:T.State.t Supply.t -> T.Var.t -> Production.t Seq.t -> M.Start.single M.t*)
val inter: M.Start.single M.t -> M.Start.single M.t -> T.State_pair.t * (Rhs.t, Lits.t) T.State_pair_graph.t
val backlog: (Rhs.t, Lits.t) T.State_pair_graph.t -> (Rhs.t, Rhs.t) T.State_pair_graph.t
val rev_solve: (Rhs.t, Rhs.t) T.State_pair_graph.t -> (Rhs.t, Rhs.t) T.State_pair_graph.t
val right_context: T.State_pair.t * (Rhs.t, Rhs.t) T.State_pair_graph.t -> T.State.t -> T.State.t -> Rhs.t

val erase_call: M.Start.multiple M.t -> M.Start.multiple M.t
val erase_return: 'a M.t -> 'a M.t
val collapse: supply:T.State.t Supply.t -> _ M.t -> M.Start.single M.t
val subset: supply:T.State.t Supply.t -> _ M.t -> M.Start.single M.t
val minimize: supply:T.State.t Supply.t -> M.Start.single M.t -> M.Start.single M.t
val earley: supply:T.State.t Supply.t -> M.Start.multiple M.t -> M.Start.single M.t

val to_start: _ M.t -> to_start

val extract_multiple: supply:T.State.t Supply.t -> (T.Labeled_var.t * M.Start.single M.t) Seq.t -> to_start -> _ M.t -> ((T.Labeled_var.t * T.State.t) * M'.Start.single M'.t) Seq.t

val strip: supply:T.State.t Supply.t -> M'.Start.single M'.t -> M.Start.single M.t

type rtn =
  {
    start: T.State.t;
    final: T.States.t;
    graph: (Rhs.t T.Labeled_var_to.t, Lits.t) T.State_graph.t
  }

val rtn_to_string: rtn -> string

val lookahead: (T.State.t -> T.State.t -> Rhs.t) -> M.Start.single M.t -> rtn

