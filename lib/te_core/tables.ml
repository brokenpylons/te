open Te_bot
open! Prelude 
module T = Types
open Tn

module Actions = struct
  type t =
    {
      accept: bool;
      shift: bool;
      reduce: T.Reductions.t;
    }
  [@@deriving eq, ord]

  let union x y =
    {
      accept = x.accept || y.accept;
      shift = x.shift || y.shift;
      reduce = T.Reductions.union x.reduce y.reduce;
    }

  let empty =
    {
      accept = false;
      shift = false;
      reduce = T.Reductions.empty;
    }

  let is_empty x =
    not x.accept &&
    not x.shift &&
    T.Reductions.is_empty x.reduce

  let accept =
    {empty with accept = true}

  let shift =
    {empty with shift = true}

  let reduce x =
    {empty with reduce = x}
end

module Actions_multimap = Lits_multimap(Actions)
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

  val is_kernel: item -> bool
  val is_reduce: item -> bool
  val is_right_nulled: item -> bool
  val shift: item -> Lits.t
  val output: item -> T.Labeled_var.t
  val distance: item -> Size.t
  val shift_lookahead: item -> Lits.t
  val reduce_lookahead: item -> Lits.t
  val filter: item -> Lits.t
  val filter2: item -> Lits_multimap(Lits).t
end

module type EXTRA = sig
  val adjacency_restriction: Lits_multimap(Lits).t
  val is_token: T.Labeled_var.t -> bool
end

(*module Make(A: Fa.FA)(Items: ITEMS)(Extra: EXTRA) = struct

  let select_strategy x =
    if Items.is_kernel x then
      if Items.is_right_nulled x then
        if Extra.is_token @@ Items.output x then
          (assert (Items.is_reduce x);
           Some T.Reduction.Strategy.Token)
        else
          match Size.to_int @@ Items.distance x with
          | Some d -> Some (T.Reduction.Strategy.Fixed d)
          | None -> failwith "Variable length"
      else
        None
    else if Items.is_reduce x then
      Some T.Reduction.Strategy.Null
    else
      None

  let actions_of_item x =
    let shift =
      let ls = Items.shift x in
      if not (Lits.is_empty ls)
      then Actions_multimap.singleton_multiple ls Actions.shift
      else Actions_multimap.empty
    in
    let reduce =
      match select_strategy x with
      | Some strategy ->
        Actions_multimap.singleton_multiple (Items.reduce_lookahead x)
          (Actions.reduce (T.Reductions.singleton (T.Reduction.make (Items.output x) strategy)))
      | None -> Actions_multimap.empty
    in
    Actions_multimap.union shift reduce

  let actions_of_items items =
    Items.to_seq items
    |> Seq.map actions_of_item
    |> Seq.fold_left Actions_multimap.union Actions_multimap.empty

  let actions g =
    M.states_labels g
    |> Seq.map (fun (s, items) -> (s, actions_of_items items))
    |> T.State_to.of_seq
end*)
