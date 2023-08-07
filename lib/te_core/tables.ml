open Te_bot
open! Prelude 
module T = Types
open Tn

module Actions = struct
  type t =
    {
      accept: bool;
      shift: bool;
      shift_code: bool;
      orders: T.Vars.t;
      matches: T.Labeled_vars.t;
      predictions: T.Labeled_vars.t;
      null: T.Reductions.t;
      reduce: T.Reductions.t;
    }
  [@@deriving eq, ord, show]

  let union x y =
    {
      accept = x.accept || y.accept;
      shift = x.shift || y.shift;
      shift_code = x.shift_code || y.shift_code;
      orders = T.Vars.union x.orders y.orders;
      matches = T.Labeled_vars.union x.matches y.matches;
      predictions = T.Labeled_vars.union x.predictions y.predictions;
      null = T.Reductions.union x.null y.null;
      reduce = T.Reductions.union x.reduce y.reduce;
    }

  let empty =
    {
      accept = false;
      shift = false;
      shift_code = false;
      orders = T.Vars.empty;
      matches = T.Labeled_vars.empty;
      predictions = T.Labeled_vars.empty;
      null = T.Reductions.empty;
      reduce = T.Reductions.empty;
    }

  let is_empty x =
    not x.accept &&
    not x.shift &&
    T.Vars.is_empty x.orders &&
    T.Labeled_vars.is_empty x.matches &&
    T.Labeled_vars.is_empty x.predictions &&
    T.Reductions.is_empty x.null &&
    T.Reductions.is_empty x.reduce

  let accept =
    {empty with accept = true}

  let shift =
    {empty with shift = true}

  let shift_code =
    {empty with shift_code = true}

  let orders x =
    {empty with orders = x}

  let matches x =
    {empty with matches = x}

  let predictions x =
    {empty with predictions = x}

  let null x =
    {empty with null = x}

  let reduce x =
    {empty with reduce = x}
end

module Actions_multimap = struct
  include Lits_multimap(Actions)
  let pp = pp Actions.pp
end
module Goto_partial_map = struct
  include Lits_multimap(T.Statess)
  let pp = pp T.Statess.pp
end
module Back_map = struct
  include Multimap.Make1(T.State_to)(T.State_pair_partial)
  let pp = T.State_to.pp T.State_pair_partial.pp
end

module type ITEMS = sig
  type item
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val empty: t
  val union: t -> t -> t
  (*val pp: t Fmt.t*)
  val to_seq: t -> item Seq.t

  val is_dead: item -> bool
  val is_kernel: item -> bool
  val is_reduce: item -> bool
  val is_right_nulled: item -> bool

  val shift: item -> Lits.t
  val null: item -> (T.Labeled_var.t * Lits.t) Seq.t
  val output: item -> T.Labeled_var.t

  val distance: item -> Size.t
  val state_pair: item -> T.State.t * T.State.t
  val first: item -> T.Var.t -> Lits.t
  val shift_code: item -> bool
  val shift_lookahead: item -> Lits.t
  val reduce_lookahead: item -> Lits.t
  val reminder: item -> T.Var.t list list

  (*val filter: item -> Lits.t
  val filter2: item -> Lits_multimap(Lits).t*)
end

(*module type EXTRA = sig
  (*val adjacency_restriction: Lits_multimap(Lits).t*)
  val tokens: T.Labeled_vars.t
end*)

module Make(A: Fa.S0 with type state = T.States.t)(B: Fa.S0 with type state = T.State_pair.t)(Items: ITEMS) = struct

  let select_strategy ~tokens x =
    if not (T.Vars.mem (T.Labeled_var.var @@ Items.output x) tokens) && Items.is_kernel x && Items.is_right_nulled x then
      match Size.to_int @@ Items.distance x with
      | Some d -> Some (T.Reduction.Strategy.Fixed d)
      | None -> Some (T.Reduction.Strategy.Scan (Items.state_pair x))
    else
      None

  let actions_of_item ~tokens x =
    let shift =
      let ls = Items.shift x in
      if not (Lits.is_empty ls)
      then Actions_multimap.singleton_multiple ls Actions.shift
      else Actions_multimap.empty
    in
    let orders =
      let ls = Items.shift x in
      if not (Lits.is_empty ls)
      then
        T.Vars.fold (fun v acc ->
            Actions_multimap.union acc @@
            Actions_multimap.singleton_multiple (Items.first x v) (Actions.orders (T.Vars.singleton v)))
           Actions_multimap.empty ls.Lits.vars
      else Actions_multimap.empty
    in
    let shift_code =
      if Items.shift_code x then
        Actions_multimap.singleton_multiple (Items.shift_lookahead x) Actions.shift_code
      else Actions_multimap.empty
    in
    let matches =
      if T.Vars.mem (T.Labeled_var.var @@ Items.output x) tokens && Items.is_reduce x && Items.is_kernel x then
        Actions_multimap.singleton_multiple (Items.reduce_lookahead x)
          (Actions.matches (T.Labeled_vars.singleton (Items.output x)))
      else Actions_multimap.empty
    in
    let predictions =
      if T.Vars.mem (T.Labeled_var.var @@ Items.output x) tokens && not (Items.is_dead x) && Items.is_kernel x then
        Actions_multimap.singleton_multiple (Items.shift_lookahead x)
          (Actions.predictions (T.Labeled_vars.singleton (Items.output x)))
      else Actions_multimap.empty
    in
    let null =
      if not (T.Vars.mem (T.Labeled_var.var @@ Items.output x) tokens) && not (Items.is_kernel x) && Items.is_right_nulled x then
        Actions_multimap.singleton_multiple (Items.reduce_lookahead x)
          (Actions.null (T.Reductions.singleton (T.Reduction.make (Items.output x) Null (Items.reminder x))))
      else Actions_multimap.empty
    in
    let shift_null =
      Items.null x
      |> Seq.map (fun (output, la) ->
          Actions_multimap.singleton_multiple la
            (Actions.null (T.Reductions.singleton (T.Reduction.make output Null [[]]))))
      |> Seq.fold_left Actions_multimap.union Actions_multimap.empty
    in
    let reduce =
      match select_strategy ~tokens x with
      | Some strategy ->
        Actions_multimap.singleton_multiple (Items.reduce_lookahead x)
          (Actions.reduce (T.Reductions.singleton (T.Reduction.make (Items.output x) strategy (Items.reminder x))))
      | None -> Actions_multimap.empty
    in
    Actions_multimap.(shift <|> orders <|> shift_code <|> matches <|> predictions <|> null <|> shift_null <|> reduce)

  let actions_of_items ~tokens items =
    Items.to_seq items
    |> Seq.map (actions_of_item ~tokens)
    |> Seq.fold_left Actions_multimap.union Actions_multimap.empty

  let actions ~tokens g =
    A.states_labels g
    |> Seq.map (fun (s, items) -> (s, actions_of_items ~tokens items))
    |> T.States_to.of_seq

  let goto g =
    A.adjacency_list g
    |> Seq.map (fun ((s, _), adj) ->
        Fmt.pr ">>>%a (%a)@." T.States.pp s (Fmt.seq (Fmt.pair T.States.pp Lits.pp)) adj;
        (s, adj
            |> Seq.map (fun (p, x) -> (x, T.Statess.singleton p))
            |> Goto_partial_map.of_seq_multiple))
    |> T.States_to.of_seq

  let back b =
    B.adjacency_list b
    |> Seq.map (fun ((s, _), adj) ->
        (s, adj
            |> Seq.map (fun (((s, _) as p), _) -> (s, Some p))
            |> Back_map.of_seq_multiple))
    |> T.State_pair_to.of_seq

  module Unoptimized: sig
    include Gsglr.TABLES
    val make: tokens:T.Vars.t -> (A.Start.single, Items.t, Lits.t) A.t -> (B.Start.multiple, _, _) B.t -> t
    val pp: t Fmt.t
  end = struct
    type actions = Actions.t
    type t =
      {
        start: T.States.t;
        goto: Goto_partial_map.t T.States_to.t;
        actions: Actions_multimap.t T.States_to.t;
        back: Back_map.t T.State_pair_to.t;
      }
    [@@deriving show]

    let make ~tokens g b =
      {
        start = A.start g;
        goto = goto g;
        actions = actions ~tokens g;
        back = back b;
      }

    let actions_union = Actions.union
    let actions_empty = Actions.empty

    let lits_of_symbol = function
      | T.Symbol.Eof -> Lits.eof
      | T.Symbol.Code x -> Lits.code x
      | T.Symbol.Var x -> Lits.var x
      | T.Symbol.Null -> Lits.null

    let start t =
      t.start

    let actions t s x =
      t.actions
      |> T.States_to.find s
      |> Actions_multimap.find_multiple_or_empty (lits_of_symbol x)

    let goto t s x =
      t.goto
      |> T.States_to.find s
      |> Goto_partial_map.find_multiple_or_empty (lits_of_symbol x)

    let back t p s =
      t.back
      |> T.State_pair_to.find p
      |> Back_map.find_multiple_or ~default:None s

    let shift a =
      a.Actions.shift

    let shift_code a =
      a.Actions.shift_code

    let orders a =
      a.Actions.orders

    let reduce a =
      a.Actions.reduce

    let null a =
      a.Actions.null

    let matches a =
      a.Actions.matches

    let predictions a =
      a.Actions.predictions
  end
end

module V1 = Make(M'')(M')(Builder.Items')
