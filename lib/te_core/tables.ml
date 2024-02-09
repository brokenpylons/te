open Te_bot
open! Prelude 
module T = Types
open Tn

module Actions_multimap = struct
  include Lits_multimap(T.Actions)
  let pp = pp T.Actions.pp
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
  val shift_code: item -> Lits.t
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
      then Actions_multimap.singleton_multiple ls T.Actions.shift
      else Actions_multimap.empty
    in
    let orders =
      let ls = Items.shift x in
      if not (Lits.is_empty ls)
      then
        T.Vars.fold (fun v acc ->
            if (T.Vars.mem v tokens) then begin
              Actions_multimap.union acc @@
              Actions_multimap.singleton_multiple (Items.first x v) (T.Actions.orders (T.Vars.singleton v))
            end else 
              acc)
           Actions_multimap.empty ls.Lits.vars
      else Actions_multimap.empty
    in
    let shift_code =
      let ls = Items.shift_code x in
      if not (Lits.is_empty ls)
      then Actions_multimap.singleton_multiple ls T.Actions.load
      else Actions_multimap.empty
    in
    let matches =
      if T.Vars.mem (T.Labeled_var.var @@ Items.output x) tokens && Items.is_reduce x && Items.is_kernel x then
        Actions_multimap.singleton_multiple (Items.reduce_lookahead x)
          (T.Actions.matches (T.Labeled_vars.singleton (Items.output x)))
      else Actions_multimap.empty
    in
    let predictions =
      if T.Vars.mem (T.Labeled_var.var @@ Items.output x) tokens && not (Items.is_dead x) && Items.is_kernel x then
        Actions_multimap.singleton_multiple (Items.shift_lookahead x)
          (T.Actions.predictions (T.Vars.singleton (T.Labeled_var.var (Items.output x))))
      else Actions_multimap.empty
    in
    let null =
      if not (T.Vars.mem (T.Labeled_var.var @@ Items.output x) tokens) && not (Items.is_kernel x) && Items.is_right_nulled x then
        Actions_multimap.singleton_multiple (Items.reduce_lookahead x)
          (T.Actions.null (T.Reductions.singleton (T.Reduction.make (Items.output x) Null (Lists (Items.reminder x)))))
      else Actions_multimap.empty
    in
    let shift_null =
      Items.null x
      |> Seq.map (fun (output, la) ->
          Actions_multimap.singleton_multiple la
            (T.Actions.null (T.Reductions.singleton (T.Reduction.make output Null (Lists [[]])))))
      |> Seq.fold_left Actions_multimap.union Actions_multimap.empty
    in
    let reduce =
      match select_strategy ~tokens x with
      | Some strategy ->
        Actions_multimap.singleton_multiple (Items.reduce_lookahead x)
          (T.Actions.reduce (T.Reductions.singleton (T.Reduction.make (Items.output x) strategy (Lists (Items.reminder x)))))
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
    type actions = T.Actions.t
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

    let lits_of_symbol = function
      | T.Symbol.Eof -> Lits.eof
      | T.Symbol.Code x -> Lits.code x
      | T.Symbol.Var x -> Lits.var x
      | T.Symbol.Delegate -> Lits.null

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
      a.T.Actions.shift

    let load a =
      a.T.Actions.load

    let orders a =
      a.T.Actions.orders

    let reduce a =
      a.T.Actions.reduce

    let null a =
      a.T.Actions.null

    let matches a =
      a.T.Actions.matches

    let predictions a =
      a.T.Actions.predictions
  end
end

module V1 = Make(M'')(M')(Builder.Items')
