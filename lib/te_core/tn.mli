open Te_bot
module T = Types

module Lits: sig
  type vars = T.Vars.t
  type t
  include Re.LITS with type t := t with type vars := vars
  include Refine.PARTITION with type t := t
  val is_return: t -> bool
  val is_nullable: (T.Var.t -> bool) -> t -> bool

  val eof: t
  val empty: t
  val null: t
  val call: T.Vars.t -> t
  val return: T.Vars.t -> t
  val var: T.Var.t -> t
  val vars: T.Vars.t -> t
  val codes: T.Codes.t -> t
end

module type SET = sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool

  val empty: t
  val union: t -> t -> t
end

module type MULTIMAP = sig
  type key
  type values
  type t

  val equal: t -> t -> bool
  val compare: t -> t -> int

  val empty: t
  val add_multiple: key -> values -> t -> t
  val singleton_multiple: key -> values -> t
  val union: t -> t -> t
  val find_multiple: key -> t -> values
  val find_multiple_or_empty: key -> t -> values
end

module Lits_multimap(S: SET): MULTIMAP with type key = Lits.t and type values = S.t

module Enhanced_lits: sig
  include Re.LITS with type t = Lits.t T.State_to.t
end

module Enhanced_lits_multimap(S: SET): sig
  include MULTIMAP with type key = Enhanced_lits.t and type values = S.t
  val strip: t -> Lits_multimap(S).t
end

module Rhs: Re.CONCRETE with type vars = T.Vars.t and type lits = Lits.t 

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

module type LITS = sig
  type vars
  type t
  include Re.LITS with type t := t and type vars := vars

  val union: t -> t -> t
  val empty: t
end

type enhanced_labeled_var

module Analysis(L: LITS)(M: Fa.FA with type lits = L.t)(_: functor(S: SET) -> MULTIMAP with type key = L.t with type values = S.t): sig

  module Nullable: sig
    module A: MULTIMAP with type key = L.t with type values = bool
    module type S = sig
      val extend: M.Start.single M.t -> A.t -> T.State.t -> A.values
      val extend_multiple: ('a * M.Start.single M.t) Seq.t -> A.t -> ('a * (T.State.t -> A.values)) Seq.t

      val compute: M.Start.single M.t -> A.t -> T.State.t -> A.values
      val compute_multiple: (L.vars * M.Start.single M.t) Seq.t -> A.t -> A.t

      val fixedpoint: (L.vars * M.Start.single M.t) Seq.t -> A.t -> A.t
    end

    val get: (module S)
  end

  module First: sig
    module A: MULTIMAP with type key = L.t with type values = L.t
    module type S = sig
      val extend: M.Start.single M.t -> A.t -> T.State.t -> A.values
      val extend_multiple: ('a * M.Start.single M.t) Seq.t -> A.t -> ('a * (T.State.t -> A.values)) Seq.t

      val compute: M.Start.single M.t -> A.t -> T.State.t -> A.values
      val compute_multiple: (L.vars * M.Start.single M.t) Seq.t -> A.t -> A.t

      val fixedpoint: (L.vars * M.Start.single M.t) Seq.t -> A.t -> A.t
    end

    val get: (L.t -> bool) -> (module S)
  end

  module Follow: sig
    module A: MULTIMAP with type key = L.t with type values = L.t
    module type S = sig
      val compute_multiple: (enhanced_labeled_var * M.Start.single M.t) Seq.t -> A.t -> A.t
      val fixedpoint: (enhanced_labeled_var * M.Start.single M.t) Seq.t -> A.t -> A.t
    end

    val get: (enhanced_labeled_var -> T.State.t -> bool) -> (enhanced_labeled_var -> T.State.t -> L.t) -> (enhanced_labeled_var -> L.t) -> (module S)
  end
end


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
val first: (Rhs.t -> bool) -> Production.t Seq.t -> Lits.t T.Var_to.t
val productions: Production.t Seq.t -> (T.Vars.t -> Rhs.t)

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

type enhanced_vars

val extract_multiple: supply:T.State.t Supply.t -> (T.Labeled_var.t * M.Start.single M.t) Seq.t -> to_start -> _ M.t -> (enhanced_labeled_var * M'.Start.single M'.t) Seq.t

val first2: (enhanced_labeled_var * M'.Start.single M'.t) Seq.t -> unit

val strip: supply:T.State.t Supply.t -> M'.Start.single M'.t -> M.Start.single M.t

type rtn =
  {
    start: T.State.t;
    final: T.States.t;
    graph: (Rhs.t T.Labeled_var_to.t, Lits.t) T.State_graph.t
  }

val rtn_to_string: rtn -> string

val lookahead: (T.State.t -> T.State.t -> Rhs.t) -> M.Start.single M.t -> rtn

