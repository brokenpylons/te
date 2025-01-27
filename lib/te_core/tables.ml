open Te_bot
open! Prelude 
module T = Types
open Tn

module Goto_partial_map = struct
  include Lits_multimap(T.States)
  let pp = pp T.States.pp
end
module Back_map = struct
  include Multimap.Make1(T.State_to)(T.State_pairs)
  let pp = T.State_to.pp T.State_pairs.pp
end

let actions lexical lookahead' nullable' a =
  A.states a
  |> T.States.to_seq
  |> Seq.map (fun s -> 
      (s, Tn.actions lexical lookahead' nullable' s a
          |> Seq.map (fun (lts, x) -> Tn.Actions_multimap.singleton_multiple lts x)
          |> Seq.fold_left Tn.Actions_multimap.union Tn.Actions_multimap.empty))
  |> T.State_to.of_seq

let goto a =
  A.to_segments a
  |> Seq.map (fun ((s, _), adj) ->
      (s, adj
          |> Seq.map (fun (p, lts) -> (lts, T.States.singleton p))
          |> Goto_partial_map.of_seq_multiple))
  |> T.State_to.of_seq

let orders lexical a =
  A.to_segments a
  |> Seq.map (fun ((s, _), adj) ->
      (s, adj
          |> Seq.map (fun (_, lts) -> T.Vars.inter lexical (Lits.to_vars lts))
          |> Seq.fold_left T.Vars.union T.Vars.empty))
  |> T.State_to.of_seq

let back b =
  PA.to_segments b
  |> Seq.map (fun ((s, _), adj) ->
      (s, adj
          |> Seq.map (fun (((s, _) as p), _) -> (s, T.State_pairs.singleton p))
          |> Back_map.of_seq_multiple))
  |> T.State_pair_to.of_seq


module Unoptimized = struct
  type actions = T.Actions.t
  type t =
    {
      start: T.State.t;
      goto: Goto_partial_map.t T.State_to.t;
      orders: T.Vars.t T.State_to.t;
      actions: Actions_multimap.t T.State_to.t;
      back: Back_map.t T.State_pair_to.t;
    }
  [@@deriving show]

  let make lexical lookahead' nullable' g b =
    {
      start = A.start g;
      goto = goto g;
      orders = orders lexical g;
      actions = actions lexical lookahead' nullable' g;
      back = back b;
    }

  let lits_of_symbol = function
    | T.Symbol.Eof -> Lits.eof
    | T.Symbol.Code x -> Lits.of_codes (T.Codes.singleton x)
    | T.Symbol.Var x -> Lits.of_vars (T.Vars.singleton x)
    | T.Symbol.Delegate -> Lits.scan'

  let start t =
    t.start

  let actions t s x =
    t.actions
    |> T.State_to.find s
    |> Actions_multimap.find_multiple_or_empty (lits_of_symbol x)

  let goto t s x =
    t.goto
    |> T.State_to.find s
    |> Goto_partial_map.find_multiple_or_empty (lits_of_symbol x)

  let orders t s =
    t.orders
    |> T.State_to.find s

  let back t p s =
    t.back
    |> T.State_pair_to.find p
    |> Back_map.find_multiple_or ~default:T.State_pairs.empty s

  let shift a =
    a.T.Actions.shift

  let load a =
    a.T.Actions.load

  let reduce a =
    a.T.Actions.reduce

  let null a =
    a.T.Actions.null

  let matches a =
    a.T.Actions.matches

  let predictions a =
    a.T.Actions.predictions
end
