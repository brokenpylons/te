open! Te_bot
open! Prelude 
module T = Types
open Tn

module Goto_partial_map = struct
  include Lits_multimap(T.State_partial)
  let pp = pp T.State_partial.pp
end

module Back_map = struct
  include Enhanced_lits_multimap(T.State_pair_partial)
  let pp = pp T.State_pair_partial.pp
end

let actions lexical lookahead' nullable' f a =
  A.states a
  |> T.States.to_seq
  |> Seq.map (fun s ->
      (s, f lexical lookahead' nullable' s a
          |> Seq.map (fun (lts, x) -> Actions_multimap.singleton_multiple lts x)
          |> Seq.fold_left Actions_multimap.union Actions_multimap.empty))
  |> T.State_to.of_seq

let goto a =
  A.to_segments a
  |> Seq.map (fun ((s, _), adj) ->
      (s, adj
          |> Seq.map (fun (p, lts) -> (lts, Some p))
          |> Goto_partial_map.of_seq_multiple))
  |> T.State_to.of_seq

let orders lexical a =
  A.to_segments a
  |> Seq.map (fun ((s, _), adj) ->
      (s, adj
          |> Seq.map (fun (_, lts) -> T.Vars.inter lexical (Lits.to_vars lts))
          |> Seq.fold_left T.Vars.union T.Vars.empty))
  |> T.State_to.of_seq

let matches lexical a =
  A.states a
  |> T.States.to_seq
  |> Seq.map (fun s ->
      (s, Tn.matches_per_state lexical s a
          |> Seq.fold_left T.Labeled_vars.union T.Labeled_vars.empty))
  |> T.State_to.of_seq

let predictions lexical a =
  A.states a
  |> T.States.to_seq
  |> Seq.map (fun s ->
      (s, Tn.predictions_per_state lexical s a
          |> Seq.fold_left T.Vars.union T.Vars.empty))
  |> T.State_to.of_seq

let valid_lookahead lexical lookahead' nullable' f a =
  A.states a
  |> T.States.to_seq
  |> Seq.map (fun s ->
      (s, f lexical lookahead' nullable' s a
          |> Seq.map (fun (lts, _) -> Lits.to_vars lts)
          |> Seq.fold_left T.Vars.union T.Vars.empty))
  |> T.State_to.of_seq

let stop b =
  PA.states_labels b
  |> Seq.map (fun (s, its) ->
      let Item.{is_kernel; _} = Items.the its in
      (s, not is_kernel))
  |> T.State_pair_to.of_seq

let back b =
  PA.to_segments b
  |> Seq.map (fun ((s, _), adj) ->
      (s, adj
          |> Seq.map (fun (p, lts) -> (lts, Some p))
          |> Back_map.of_seq_multiple))
  |> T.State_pair_to.of_seq

let accept start lookahead' a =
  A.states a
  |> T.States.to_seq
  |> Seq.map (fun s ->
      (s, Tn.accept start lookahead' s a))
  |> T.State_to.of_seq

module Unoptimized = struct
  type actions = T.Actions.t
  type t =
    {
      start: T.State.t;
      goto: Goto_partial_map.t T.State_to.t;
      orders: T.Vars.t T.State_to.t;
      actions: Actions_multimap.t T.State_to.t;
      back: Back_map.t T.State_pair_to.t;
      stop: bool T.State_pair_to.t;
      accept: bool T.State_to.t;
    }
  [@@deriving show]

  let make start lexical lookahead' nullable' g b =
    {
      start = A.start g;
      goto = goto g;
      orders = orders lexical g;
      actions = actions lexical lookahead' nullable' Tn.actions g;
      stop = stop b;
      back = back b;
      accept = accept start lookahead' g;
    }

  let lits_of_symbol = function
    | T.Symbol.Eof -> Lits.eof
    | T.Symbol.Code x -> Lits.of_codes (T.Codes.singleton x)
    | T.Symbol.Var x -> Lits.of_vars (T.Vars.singleton x)
    | T.Symbol.Delegate -> Lits.scan'

  let start t =
    t.start

  let actions t s sym =
    t.actions
    |> T.State_to.find s
    |> Actions_multimap.find_multiple_or_empty (lits_of_symbol sym)

  let goto t s sym =
    t.goto
    |> T.State_to.find s
    |> Goto_partial_map.find_multiple_or_empty (lits_of_symbol sym)

  let orders t s =
    t.orders
    |> T.State_to.find s

  let back t p s sym =
    t.back
    |> T.State_pair_to.find p
    |> Back_map.find_multiple_or_empty (Enhanced_lits.make s (lits_of_symbol sym))

  let accept t s =
    t.accept
    |> T.State_to.find s

  let stop t s =
    t.stop
    |> T.State_pair_to.find s

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

module Unoptimized_classic = struct
  type actions = T.Actions.t
  type t =
    {
      start_parser: T.State.t;
      start_scanner: T.State.t;
      goto: Goto_partial_map.t T.State_to.t;
      actions: Actions_multimap.t T.State_to.t;
      matches: T.Labeled_vars.t T.State_to.t;
      predictions: T.Vars.t T.State_to.t;
      accept: bool T.State_to.t;
      valid_lookahead: T.Vars.t T.State_to.t;
    }
  [@@deriving show]

  let make start lexical lookahead' nullable' gp gs  =
    {
      start_parser = A.start gp;
      start_scanner = A.start gs;
      goto = T.State_to.union (fun _ -> Goto_partial_map.union) (goto gp) (goto gs);
      actions = actions lexical lookahead' nullable' Tn.actions_classic gp;
      matches = matches lexical gs;
      predictions = predictions lexical gs;
      accept = accept start lookahead' gp;
      valid_lookahead = valid_lookahead lexical lookahead' nullable' Tn.actions_classic gp;
    }

  let lits_of_symbol = function
    | T.Symbol.Eof -> Lits.eof
    | T.Symbol.Code x -> Lits.of_codes (T.Codes.singleton x)
    | T.Symbol.Var x -> Lits.of_vars (T.Vars.singleton x)
    | T.Symbol.Delegate -> Lits.scan'

  let start_parser t =
    t.start_parser

  let start_scanner t =
    t.start_scanner

  let actions t s sym =
    t.actions
    |> T.State_to.find s
    |> Actions_multimap.find_multiple_or_empty (lits_of_symbol sym)

  let goto t s sym =
    t.goto
    |> T.State_to.find s
    |> Goto_partial_map.find_multiple_or_empty (lits_of_symbol sym)

  let valid_lookahead t s =
    t.valid_lookahead
    |> T.State_to.find s

  let accept t s =
    t.accept
    |> T.State_to.find s

  let shift a =
    a.T.Actions.shift

  let reduce a =
    a.T.Actions.reduce

  let null a =
    a.T.Actions.null

  let matches t s =
    t.matches
    |> T.State_to.find s

  let predictions t s =
    t.predictions
    |> T.State_to.find s
end

module Symbol_multimap(Values: SET) = struct
  module VM = Multimap.Make1(T.Var_to)(Values)
  module CM = Multimap.Make1(T.Code_to)(Values)

  type t =
    {
      eof: Values.t;
      delegate: Values.t;
      vars: VM.t;
      codes: CM.t;
      default: Values.t;
    }

  let pp pp_s ppf x =
    Fmt.pf ppf "@[@[%a@]@,@[%a@]@,@[%a@]@,@[%a@],@[%a@]@]"
      (pp_if (not @@ Values.is_empty x.eof) (fun ppf -> Fmt.pf ppf "%s %a" T.eof_string pp_s)) x.eof
      (pp_if (not @@ Values.is_empty x.delegate) (fun ppf -> Fmt.pf ppf "%s %a" T.delegate_string pp_s)) x.delegate
      (T.Var_to.pp pp_s) x.vars
      (T.Code_to.pp pp_s) x.codes
      (pp_if (not @@ Values.is_empty x.default) (fun ppf -> Fmt.pf ppf "%a" pp_s)) x.default

  let empty =
    {
      eof = Values.empty;
      delegate = Values.empty;
      vars = VM.empty;
      codes = CM.empty;
      default = Values.empty;
    }

  let add_multiple k vs t =
    match k with
    | Some T.Symbol.Eof -> {t with eof = Values.union vs t.eof}
    | Some (T.Symbol.Code x) -> {t with codes = CM.add_multiple x vs t.codes}
    | Some (T.Symbol.Var x) -> {t with vars = VM.add_multiple x vs t.vars}
    | Some T.Symbol.Delegate -> {t with delegate = Values.union vs t.delegate}
    | None -> {t with default = Values.union vs t.default}

  let union x y =
    {
      eof = Values.union x.eof y.eof;
      delegate = Values.union x.delegate y.delegate;
      vars = VM.union x.vars y.vars;
      codes = CM.union x.codes y.codes;
      default = Values.union x.default y.default;
    }

  let singleton_multiple k vs = add_multiple k vs empty

  let find k t =
    match k with
    | T.Symbol.Eof -> t.eof
    | T.Symbol.Code x ->
      (try CM.find_multiple x t.codes
       with Not_found -> t.default)
    | T.Symbol.Var x -> VM.find_multiple x t.vars
    | T.Symbol.Delegate -> t.delegate

  let find_or_empty k t =
      try find k t
      with Not_found -> Values.empty

  let add_seq_multiple s t =
    Seq.fold_left (fun t (k, v) -> add_multiple k v t) t s

  let of_seq_multiple s =
    add_seq_multiple s empty
end

module Actions_multimap' = struct
  include Symbol_multimap(T.Actions)
  let pp = pp T.Actions.pp
end

module Goto_partial_map' = struct
  include Symbol_multimap(T.State_partial)
  let pp = pp T.State_partial.pp
end

let lits_to_symbols lts = 
  let (@) = Seq.append in
  (if lts.Lits.eof then Seq.return (Some T.Symbol.Eof) else Seq.empty) @
  (if lts.Lits.scan' then Seq.return (Some T.Symbol.Delegate) else Seq.empty) @
  (match T.Codes.to_seq_opt @@ Lits.to_codes lts with
   | Some cs -> Seq.map (fun x -> Some (T.Symbol.Code x)) cs
   | None -> Seq.return None) @
  (Seq.map (fun x -> Some (T.Symbol.Var x)) @@ T.Vars.to_seq @@ Lits.to_vars lts)

let to_array empty x =
  let max =
    T.State_to.to_list x
    |> List.map (fun (s, _) -> s)
    |> List.sort (fun x y -> -T.State.compare x y)
    |> List.hd
  in
  Array.init (succ max) (fun i ->
      try T.State_to.find i x with Not_found -> empty)

let actions' lexical lookahead' nullable' a =
  A.states a
  |> T.States.to_seq
  |> Seq.map (fun s ->
      (s, Tn.actions lexical lookahead' nullable' s a
          |> Seq.flat_map (fun (lts, x) ->
              Seq.map (fun sym ->
                  Actions_multimap'.singleton_multiple sym x)
                (lits_to_symbols lts))
          |> Seq.fold_left Actions_multimap'.union Actions_multimap'.empty))
  |> T.State_to.of_seq

let goto' a =
  A.to_segments a
  |> Seq.map (fun ((s, _), adj) ->
      (s, adj
          |> Seq.flat_map (fun (p, lts) ->
              Seq.map (fun sym ->
                  (sym, Some p))
                (lits_to_symbols lts))
          |> Goto_partial_map'.of_seq_multiple))
  |> T.State_to.of_seq

module Optimized = struct
  type actions = T.Actions.t
  type t =
    {
      start: T.State.t;
      goto: Goto_partial_map'.t array;
      orders: T.Vars.t array;
      actions: Actions_multimap'.t array;
      back: Back_map.t T.State_pair_to.t;
      stop: bool T.State_pair_to.t;
      accept: bool T.State_to.t;
    }
  [@@deriving show]

  let make start lexical lookahead' nullable' g b =
    {
      start = A.start g;
      goto = to_array Goto_partial_map'.empty @@ goto' g;
      orders = to_array T.Vars.empty @@ orders lexical g;
      actions = to_array Actions_multimap'.empty @@ actions' lexical lookahead' nullable' g;
      stop = stop b;
      back = back b;
      accept = accept start lookahead' g;
    }

  let lits_of_symbol = function
    | T.Symbol.Eof -> Lits.eof
    | T.Symbol.Code x -> Lits.of_codes (T.Codes.singleton x)
    | T.Symbol.Var x -> Lits.of_vars (T.Vars.singleton x)
    | T.Symbol.Delegate -> Lits.scan'

  let start t =
    t.start

  let actions t s sym =
    t.actions.(s)
    |> Actions_multimap'.find_or_empty sym

  let goto t s sym =
    t.goto.(s)
    |> Goto_partial_map'.find_or_empty sym

  let orders t s =
    t.orders.(s)

  let back t p s sym =
    t.back
    |> T.State_pair_to.find p
    |> Back_map.find_multiple_or_empty (Enhanced_lits.make s (lits_of_symbol sym))

  let accept t s =
    t.accept
    |> T.State_to.find s

  let stop t s =
    t.stop
    |> T.State_pair_to.find s

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
