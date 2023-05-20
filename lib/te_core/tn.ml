open Te_bot
open! Prelude 
module T = Types

let pp_if b pp ppf  =
  if b then Fmt.pf ppf "%a@ " pp else Fmt.nop ppf

module Lits: sig
  type t = {eof: bool; call: bool; return: T.Vars.t; null: bool; vars: T.Vars.t; codes: T.Codes.t}
  include Re.LITS with type t := t
  include Refine.PARTITION with type t := t
  val comp: t -> t
  val is_return: t -> bool

  val eof: t
  val empty: t
  val null: t
  val call: t
  val return: T.Vars.t -> t
  val var: T.Var.t -> t
  val vars: T.Vars.t -> t
  val codes: T.Codes.t -> t
end = struct
  type t = {eof: bool; call: bool; return: T.Vars.t; null: bool; vars: T.Vars.t; codes: T.Codes.t}
  [@@deriving eq, ord]

  let pp ppf x =
    Fmt.pf ppf "@[%a%a%a%a%a%a@]"
      (pp_if x.eof Fmt.string) "$" 
      (pp_if x.call Fmt.string) "◂"
      (pp_if x.null Fmt.string) "ε" 
      (pp_if (not @@ T.Vars.is_empty x.vars) T.Vars.pp) x.vars
      (pp_if (not @@ T.Codes.is_empty x.codes) T.Codes.pp) x.codes
      (pp_if (not @@ T.Vars.is_empty x.return) (fun ppf -> Fmt.pf ppf "▸ %a" T.Vars.pp)) x.return

  let subset x y =
    Bool.imp x.eof y.eof &&
    Bool.imp x.call y.call &&
    T.Vars.subset x.return y.return &&
    Bool.imp x.null y.null &&
    T.Vars.subset x.vars y.vars && 
    T.Codes.subset x.codes y.codes

  let empty =
    {
      eof = false;
      call = false;
      return = T.Vars.empty;
      null = false;
      vars = T.Vars.empty;
      codes = T.Codes.empty;
    }

  let is_empty x =
    not x.eof && 
    not x.call && 
    T.Vars.is_empty x.return &&
    not x.null && 
    T.Vars.is_empty x.vars &&
    T.Codes.is_empty x.codes

  let is_return x =
    not (T.Vars.is_empty x.return)

  let eof = {empty with eof = true}
  let call = {empty with call = true}
  let return x = {empty with return = x}
  let null = {empty with null = true}
  let codes x = {empty with codes = x}
  let var x = {empty with vars = T.Vars.singleton x}
  let vars x = {empty with vars = x}

  let comp x =
    {
      eof = true;
      call = false;
      return = T.Vars.empty;
      null = false;
      vars = T.Vars.empty;
      codes = T.Codes.comp x.codes;
    }

  let union x y =
    {
      eof = x.eof || y.eof;
      call = x.call || y.call;
      return = T.Vars.union x.return y.return;
      null = x.null || y.null;
      vars = T.Vars.union x.vars y.vars;
      codes = T.Codes.union x.codes y.codes;
    }

  let inter x y =
    {
      eof = x.eof && y.eof;
      call = x.call && y.call;
      return = T.Vars.inter x.return y.return;
      null = x.null && y.null;
      vars = T.Vars.inter x.vars y.vars;
      codes = T.Codes.inter x.codes y.codes;
    }

  let diff x y =
    {
      eof = not (Bool.imp x.eof y.eof);
      call = not (Bool.imp x.call y.call);
      return = T.Vars.diff x.return y.return;
      null = not (Bool.imp x.null y.null);
      vars = T.Vars.diff x.vars y.vars;
      codes = T.Codes.diff x.codes y.codes;
    }
end

module Labels = struct
  type t = {states: T.States.t; predictions: T.Labeled_vars.t; matches: T.Labeled_vars.t}
  [@@deriving eq, ord]

  let pp ppf x =
    Fmt.pf ppf "@[%a%a%a@]"
      (pp_if (not @@ T.States.is_empty x.states) (fun ppf -> Fmt.pf ppf "S: %a" T.States.pp)) x.states
      (pp_if (not @@ T.Labeled_vars.is_empty x.predictions) (fun ppf -> Fmt.pf ppf "P: %a" T.Labeled_vars.pp)) x.predictions
      (pp_if (not @@ T.Labeled_vars.is_empty x.matches) (fun ppf -> Fmt.pf ppf "M: %a" T.Labeled_vars.pp)) x.matches

  let empty =
    {
      states = T.States.empty;
      predictions = T.Labeled_vars.empty;
      matches = T.Labeled_vars.empty;
    }

  let union x y =
    {
      states = T.States.union x.states y.states;
      predictions = T.Labeled_vars.union x.predictions y.predictions;
      matches = T.Labeled_vars.union x.matches y.matches;
    }

  (*let inter x y =
    {
      predictions = T.Labeled_vars.inter x.predictions y.predictions;
      matches = T.Labeled_vars.inter x.matches y.matches;
    }*)
end

module Enhanced_lits: Re.LITS = struct
  type t = Lits.t * T.State.t
  [@@deriving eq, ord]

  let pp ppf (x, q) =
    Fmt.pf ppf "@[%a(%a)@]" Lits.pp x T.State.pp q

  let subset =
    on Lits.subset fst
end

let refine xs =
  let module R = Refine.Set(Lits) in
  let f = R.refine xs in
  Seq.cons (Lits.comp (R.considered f)) (R.partitions f)

module Rhs = Re.Concrete(Lits)
module Rhs_to = Balanced_binary_tree.Map.Size(Rhs)

module Production = struct
  type t = T.Labeled_var.t * Rhs.t
  let lhs = fst
  let rhs = snd

  let make lhs rhs = (lhs, rhs)

  let pp ppf (lhs, rhs) =
    Fmt.pf ppf "@[%a ::= %a@]" T.Labeled_var.pp lhs Rhs.pp rhs
end

module M = Fa.Make(Labels)(Lits)

module Subset = M.Gen(T.State_index(Rhs_to))

(*module Grammar = struct
  type t = {start: T.Var.t; symbols: T.Vars.t; terminal: T.Vars.t; productions: Production.t list}
  [@@deriving eq, ord]

  let lhs_symbols ps =
    List.to_seq ps
    |> Seq.map (fun p -> p.Production.lhs)
    |> T.Vars.of_seq

  let rhs_symbols ps =
    List.to_seq ps
    |> Seq.map (fun p -> Seq.fold_left Lits.union Lits.empty @@ R.occur p.Production.rhs)
    |> Seq.map (fun ls -> ls.Lits.vars)
    |> Seq.fold_left T.Vars.union T.Vars.empty

  let make start ps =
    let l = lhs_symbols ps in
    let r = rhs_symbols ps in
    {start; symbols = T.Vars.union l r; terminal = T.Vars.diff r l; productions = ps}
  end*)

let label is_final is_dead lhs q =
  Labels.{
    states = T.States.singleton q;
    predictions = T.Labeled_vars.(if not is_dead then singleton lhs else empty);
    matches = T.Labeled_vars.(if is_final then singleton lhs else empty);
  }

let convert ~supply (lhs, rhs) =
  let module Gen = M.Gen(T.State_index(Rhs_to)) in
  (lhs, Gen.unfold ~supply ~merge:Lits.union (fun q rhs ->
       let is_final = Rhs.is_nullable rhs
       and is_dead = Rhs.is_nothing rhs in
       (is_final, label is_final is_dead lhs q, Seq.map (fun ls -> (ls, Rhs.simplify @@ Rhs.derivative ls rhs)) (refine @@ Rhs.first rhs)))
      rhs)

let convert_multiple ~supply ps =
  Seq.zip (Supply.split supply) ps
  |> Seq.map (fun (supply, p) -> convert ~supply p)

module Acc' = Multimap.Make(T.Var_to)
module Acc = Acc'.L2(T.States)

let construct ~supply s' ps =
  let ps' = Seq.memoize @@ convert_multiple ~supply ps in
  let starts, finals = Seq.fold_left (fun (starts, finals) (lhs, rhs) ->
      let s = T.Labeled_var.var lhs in
      Acc.add s (M.start rhs) starts, Acc.add_multiple s (M.final rhs) finals)
      (Acc.empty, Acc.empty) ps'
  in
  let t = Seq.fold_left (Fun.flip @@ M.sum % Production.rhs) M.empty ps' in
  M.{
    start = M.Start.Single (T.States.the (Acc.find_multiple s' starts));
    final = Acc.find_multiple s' finals;
    graph = M.skeleton (Seq.fold_left (fun t (from, _to_, ls) -> 
        T.Vars.fold (fun x t ->
            t
            |> M.link ~merge:Lits.union (T.States.singleton from) (Acc.find_multiple x starts) Lits.call)
            (*|> M.link ~merge:Lits.union (Acc.find_multiple x finals) (T.States.singleton to_) (Lits.return (T.Vars.singleton x)))*)
          t ls.Lits.vars)
        t (M.transitions t))
  }

module State_pairs_to = Balanced_binary_tree.Map.Size(T.State_pair)

module P = Fa.Make(T.State_pairs)(Lits)

let inter m1 m2 =
  T.State_pair_graph.unfold' (fun _ _ (start, (from1, from2)) ->
      let adj = Seq.product (M.adjacent from1 m1) (M.adjacent from2 m2)
                |> Seq.filter_map (fun ((to1, c1), (to2, c2)) ->
                    let c = Lits.inter c1 c2 in
                    if Lits.is_empty c || T.Labeled_vars.disjoint (M.labels to1 m1).predictions (M.labels to2 m2).predictions
                    then None
                    else Some (c, (to1, to2), (false, (to1, to2))))
      in
      let adj' = 
        M.adjacent from2 m2
        |> Seq.filter_map (fun (s, ls) -> if Lits.subset ls Lits.call then Some (ls, (from1, s), (true, (from1, s))) else None)
      in
      (start, Seq.(adj @ adj')))
    (M.start m1, M.start m2) (true, (M.start m1, M.start m2))

module States_to = Balanced_binary_tree.Map.Size(T.States)

let erase_call t =
  M.homomorphism (fun ls -> if Lits.subset ls Lits.call then Lits.null else ls) t

let erase_return t =
  M.homomorphism (fun ls -> if Lits.is_return ls then Lits.null else ls) t

let closure t qs =
  let module C = Closure.Make(T.States) in
  C.closure (fun q -> M.goto q Lits.call t) qs

let closure' t qs =
  let module C = Closure.Make(T.States) in
  C.closure (fun q -> M.goto q Lits.null t) qs

let collapse ~supply t =
  let module Gen = M.Gen(T.State_index(States_to)) in
  let module R = Refine.Map(Lits)(T.States) in

  Gen.unfold ~supply ~merge:Lits.union (fun _ from ->
      let from = closure' t from in
      let next =
        M.adjacent_multiple from t
        |> Seq.filter_map (fun (s, ls) -> if Lits.subset ls Lits.null then None else Some (ls, T.States.singleton s))
        |> R.refine
        |> R.partitions
      in
      (M.is_final_multiple from t, M.labels_multiple from t, next))
    (closure' t @@ M.start_multiple t)

module Labled_vars_to = Balanced_binary_tree.Map.Size(T.Labeled_vars)

let preimage t =
  let module R = Refine.Map(Lits)(T.States) in
  let r = M.rev t in fun from ->
    M.adjacent_multiple from r
    |> Seq.map (fun (s, ls) -> (ls, T.States.singleton s))
    |> R.refine
    |> R.partitions

let group_states_by_output t =
  let module R' = Refine.Map(T.Labeled_vars)(T.States) in
  M.states t
  |> T.States.to_seq
  |> Seq.map (fun s -> ((M.labels s t).matches, T.States.singleton s))
  |> R'.refine
  |> R'.partitions

let partition (t: M.Start.single M.t) = 
  let module H = Refine.Hopcroft(T.States) in

  let initial =
    Seq.cons (M.states t) (Seq.map snd @@ group_states_by_output t)
    |> Seq.fold_left (Fun.flip H.add) H.empty
  in
  let preimage = preimage t in
  let rec go h = 
    match H.pivot h with
    | Some (from, h) -> 
      go (Seq.fold_left (Fun.flip H.add) h (Seq.map snd @@ preimage from))
    | None -> h
  in
  H.partitions (go initial)

let find_partition q prs =
  List.find (T.States.mem q) prs

let minimize ~supply t =
  let module Gen = M.Gen(T.State_index(States_to)) in
  let prs = List.of_seq @@ partition t in
  let start' = find_partition (M.start t) prs in
  Gen.unfold ~supply ~merge:Lits.union (fun _ qs ->
      let adj = Seq.map (fun (q', c) -> (c, find_partition q' prs))
          (M.adjacent_multiple qs t)
      in
      (M.is_final_multiple qs t, M.labels_multiple qs t, adj))
    start'

let subset ~supply t =
  let module Gen = M.Gen(T.State_index(States_to)) in
  Gen.unfold ~supply ~merge:Lits.union (fun _ from ->
      let module R = Refine.Map(Lits)(T.States) in
      let from = closure t from in
      let next =
        M.adjacent_multiple from t
        |> Seq.filter_map (fun (s, ls) -> if Lits.subset ls Lits.call then None else Some (ls, T.States.singleton s))
        |> R.refine 
        |> R.partitions
      in
      (M.is_final_multiple from t, M.labels_multiple from t, next))
    (closure t @@ M.start_multiple t)

module Colored_states = struct
  type t = bool * T.States.t
  [@@deriving ord]
end

module Colored_states_to = Balanced_binary_tree.Map.Size(Colored_states)

let earley ~supply t =
  let module Gen = M.Gen(T.State_index(Colored_states_to)) in
  Gen.unfold ~supply ~merge:Lits.union (fun _ (null, from) ->
      let module R = Refine.Map(Lits)(T.States) in
      let next =
        if null then
          M.adjacent_multiple from t
          |> Seq.filter_map (fun (s, ls) -> if Lits.subset ls Lits.call then None else Some (ls, T.States.singleton s))
          |> R.refine 
          |> R.partitions
          |> Seq.map (fun (ls, to_) -> (ls, (false, to_)))
        else begin
          let calls, others =
            M.adjacent_multiple from t
            |> Seq.map (fun (to_, ls) -> (ls, T.States.singleton to_))
            |> R.refine 
            |> R.partitions
            |> Seq.partition (fun (ls, _) -> Lits.subset ls Lits.call)
          in
          Seq.(Seq.map (fun (ls, to_) -> (ls, (false, to_))) others @ Seq.map (fun (ls, to_) -> (ls, (true, closure t to_))) calls)
        end
      in
      (M.is_final_multiple from t, M.labels_multiple from t, next))
    (false, M.start_multiple t)


