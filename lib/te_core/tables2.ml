(*open Te_bot
open! Prelude
module T = Types

module type SLICE = sig
  type t
  type alphabet
  val states: t -> T.State_pairs.t
  val adjacent: t -> T.State_pair.t -> (T.State_pair.t * alphabet) Seq.t
  val final: t -> T.State_pairs.t
end

module type TN = sig
  module Reminder: SLICE with type alphabet = T.Var.t
  module Backlinks: SLICE with type alphabet = T.State.t

  type t
  val syntactic: t -> T.Vars.t
  val lexical: t -> T.Vars.t
  val terminal: t -> T.Vars.t

  val states: t -> T.States.t
  val start: t -> T.State.t
  val adjacent: t -> T.State.t -> (T.State.t * T.Symbols.t) Seq.t
  val load: t -> T.State.t -> T.Symbols.t
  val final: t -> T.States.t
  val output: t -> T.State.t -> T.Labeled_vars.t
  val null_output: t -> T.State.t -> T.Labeled_vars.t
  val predictions: t -> T.State.t -> T.Labeled_vars.t

  val state_lookahead: t -> T.State.t -> T.Symbols.t
  val shift_lookahead: t -> T.State.t -> T.Symbol.t -> T.Symbols.t
  val reduce_lookahead: t -> T.State.t -> T.Labeled_var.t -> T.Symbols.t
  val reminder: t -> T.State.t -> T.Labeled_var.t -> T.State_pair.t option
  val backlinks: t -> T.State.t -> T.Labeled_var.t -> T.State_pair.t option
  val distance: t -> T.State.t -> T.Labeled_var.t -> Size.t
end

module Actions_multimap = struct
  include T.Symbols_multimap(T.Actions)
  let pp = pp T.Actions.pp
end
module Goto_partial_map = struct
  include T.Symbols_multimap(T.Statess)
  let pp = pp T.Statess.pp
end
module Back_map = struct
  include Multimap.Make1(T.State_to)(T.State_pair_partial)
  let pp = T.State_to.pp T.State_pair_partial.pp
end

module Make(Tn: TN) = struct

  let select_terminal n syms =
    let codes = T.Symbols.to_codes syms in
    let terminal_vars = T.Vars.inter (Tn.terminal n) (T.Symbols.to_vars syms) in
    T.Symbols.union (T.Symbols.of_codes codes) (T.Symbols.of_vars terminal_vars)

  let state_actions n s =
    let shift =
      Seq.fold_left Actions_multimap.union Actions_multimap.empty @@
      Seq.map (fun (_, syms) ->
          Actions_multimap.singleton_multiple syms T.Actions.shift)
        (Tn.adjacent n s)
    in
    let orders =
      Seq.fold_left Actions_multimap.union Actions_multimap.empty @@
      Seq.flat_map (fun (_, syms) ->
          Seq.map (fun var ->
              Actions_multimap.singleton_multiple (Tn.shift_lookahead n s (T.Symbol.Var var)) (T.Actions.orders (T.Vars.singleton var)))
            (T.Vars.to_seq @@ T.Symbols.to_vars syms))
        (Tn.adjacent n s)
    in
    let load =
      (*if not (Tn.is_scanner n s) then
        Actions_multimap.singleton_multiple (select_terminal n @@ Tn.state_lookahead n s) T.Actions.load
      else*)
        Actions_multimap.empty
    in
    let matches =
      Seq.fold_left Actions_multimap.union Actions_multimap.empty @@
      Seq.map (fun lv ->
          if T.Vars.mem (T.Labeled_var.var lv) (Tn.lexical n) then
            Actions_multimap.singleton_multiple (Tn.reduce_lookahead n s lv) (T.Actions.matches (T.Labeled_vars.singleton lv))
          else
            Actions_multimap.empty)
        (T.Labeled_vars.to_seq @@ Tn.output n s)
    in
    let predictions =
      Seq.fold_left Actions_multimap.union Actions_multimap.empty @@
      Seq.map (fun lv ->
          if T.Vars.mem (T.Labeled_var.var lv) (Tn.lexical n) then
            Actions_multimap.singleton_multiple (Tn.state_lookahead n s) (T.Actions.matches (T.Labeled_vars.singleton lv))
          else
            Actions_multimap.empty)
        (T.Labeled_vars.to_seq @@ Tn.output n s)
    in
    let null_reduce =
      Seq.fold_left Actions_multimap.union Actions_multimap.empty @@
      Seq.map (fun lv ->
          Actions_multimap.singleton_multiple (Tn.reduce_lookahead n s lv)
            (T.Actions.null (T.Reductions.singleton (T.Reduction.make lv Null (Gen !!(Tn.reminder n s lv))))))
        (T.Labeled_vars.to_seq @@ Tn.null_output n s)
    in
    let null_shift =
      Seq.fold_left Actions_multimap.union Actions_multimap.empty @@
      Seq.flat_map (fun (q, syms) ->
          if syms.T.Symbols.delegate then
            Seq.map (fun lv ->
                Actions_multimap.singleton_multiple (Tn.shift_lookahead n s (T.Symbol.Var (T.Labeled_var.var lv)))
                  (T.Actions.null (T.Reductions.singleton (T.Reduction.make lv Null Complete))))
              (T.Labeled_vars.to_seq @@ Tn.null_output n q)
          else
            Seq.return Actions_multimap.empty)
        (Tn.adjacent n s)
    in
    let reduce =
      Seq.fold_left Actions_multimap.union Actions_multimap.empty @@
      Seq.map (fun lv ->
          let strategy =
            let open T.Reduction.Strategy in
            match Size.to_int @@ Tn.distance n s lv with
            | Some d -> Fixed d
            | None ->
              match Tn.backlinks n s lv with
              | Some p -> Scan p
              | None -> assert false
          in
          let reminder =
            let open T.Reduction.Reminder in
            match Tn.reminder n s lv with
            | Some p -> Gen p
            | None -> Complete
          in
          Actions_multimap.singleton_multiple (Tn.reduce_lookahead n s lv)
            (T.Actions.null (T.Reductions.singleton (T.Reduction.make lv strategy reminder))))
        (T.Labeled_vars.to_seq @@ Tn.output n s)
    in
    Actions_multimap.(shift <|> orders <|> load <|> matches <|> predictions <|> null_reduce <|> null_shift <|> reduce)

  let actions n =
    Tn.states n
    |> T.States.to_seq
    |> Seq.map (fun s -> (T.States.singleton s, state_actions n s))
    |> T.States_to.of_seq

  let goto n =
    Tn.states n
    |> T.States.to_seq
    |> Seq.map (fun s ->
        (T.States.singleton s, Tn.adjacent n s
                               |> Seq.map (fun (p, x) -> (x, T.Statess.singleton (T.States.singleton p)))
                               |> Goto_partial_map.of_seq_multiple))
    |> T.States_to.of_seq

  let back b =
    Tn.Backlinks.states b
    |> T.State_pairs.to_seq
    |> Seq.map (fun s ->
        (s, Tn.Backlinks.adjacent b s
            |> Seq.map (fun (((s, _) as p), _) -> (s, Some p))
            |> Back_map.of_seq_multiple))
    |> T.State_pair_to.of_seq

  module Unoptimized: sig
    include Gsglr.TABLES
    val make: Tn.t -> Tn.Reminder.t -> Tn.Backlinks.t -> t
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

    let make n _ b =
      {
        start = T.States.singleton (Tn.start n);
        goto = goto n;
        actions = actions n;
        back = back b;
      }

    let start t =
      t.start

    let actions t s x =
      t.actions
      |> T.States_to.find s
      |> Actions_multimap.find_multiple_or_empty (T.Symbols.singleton x)

    let goto t s x =
      t.goto
      |> T.States_to.find s
      |> Goto_partial_map.find_multiple_or_empty (T.Symbols.singleton x)

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
end*)
