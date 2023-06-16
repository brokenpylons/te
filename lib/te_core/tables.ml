open Te_bot
open! Prelude 
module T = Types
open Tn

module Reduction = struct
  module Strategy = struct
    type t = Null | Token | Fixed of int | Scan of unit
    [@@deriving eq, ord]
  end

  type t = {output: T.Labeled_var.t; strategy: Strategy.t}
  [@@deriving eq, ord]
end
module Reductions = Balanced_binary_tree.Set.Size(Reduction)

module Actions = struct
  type t =
    {
      accept: bool;
      shift: bool;
      reduce: Reductions.t;
    }
  [@@deriving eq, ord]

  let union x y =
    {
      accept = x.accept || y.accept;
      shift = x.shift || y.shift;
      reduce = Reductions.union x.reduce y.reduce;
    }

  let empty =
    {
      accept = false;
      shift = false;
      reduce = Reductions.empty;
    }

  let is_empty x =
    not x.accept &&
    not x.shift &&
    Reductions.is_empty x.reduce

  let accept =
    {empty with accept = true}

  let shift =
    {empty with shift = true}

  let reduce x =
    {empty with reduce = x}
end

module Actions_multimap = Lits_multimap(Reductions)
module Goto_partial_map = Lits_multimap(T.State_partial)

module type ITEMS = sig
  type item
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val empty: t
  val union: t -> t -> t
  val pp: t Fmt.t
  val to_seq: t -> item Seq.t

  val is_nullable: item -> bool
  val is_final: item -> bool
  val is_right_nulled: item -> bool
  val output: item -> T.Labeled_var.t
  val distance: item -> Size.t
  val shift_lookahead: item -> Lits.t
  val reduce_lookahead: item -> Lits.t
  val filter: item -> Lits.t
  val filter2: item -> Lits_multimap(Lits).t
end

module type MODIFIERS = sig
  val adjacency_restriction: Lits_multimap(Lits).t
  val is_token: T.Labeled_var.t -> bool
end

module Make(A: Fa.FA)(Items: ITEMS)(Modifiers: MODIFIERS) = struct

  let reduce x =

  let actions items =
    Items.to_seq items
    |> Seq.map (fun x ->
        match Size.to_int @@ Items.distance x with
        | Some d -> (Items.reduce_lookahead x, Reductions.singleton {output = Items.output x; strategy = Fixed d})
        | None -> failwith "Variable length"





      )




end
