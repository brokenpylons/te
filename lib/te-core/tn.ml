open Te_bot
open! Prelude 
module T = Types

module Lits: sig
  type t = {call: bool; return: bool; null: bool; vars: T.Vars.t; codes: T.Codes.t}
  include Re.LITS with type t := t
  val empty: t
  val null: t
  val call: t
  val return: t
  val var: T.Var.t -> t
  val codes: T.Codes.t -> t

  val is_empty: t -> bool
  val comp: t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
end = struct
  type t = {call: bool; return: bool; null: bool; vars: T.Vars.t; codes: T.Codes.t}
  [@@deriving eq, ord]

  let pp ppf x =
    Fmt.pf ppf "@[%b@,%b@,%b@,%a@,%a@]" x.call x.return x.null T.Vars.pp x.vars T.Codes.pp x.codes

  let subset x y =

    T.Vars.subset x.vars y.vars && T.Codes.subset x.codes y.codes

  let empty =
    {
      call = false;
      return = false;
      null = false;
      vars = T.Vars.empty;
      codes = T.Codes.empty;
    }

  let is_empty x =
    not x.call && not x.return && not x.null && T.Vars.is_empty x.vars && T.Codes.is_empty x.codes

  let call = {empty with call = true}
  let return = {empty with return = true}
  let null = {empty with null = true}
  let codes x = {empty with codes = x}
  let var x = {empty with vars = T.Vars.singleton x}

  let comp x =
    {
      call = false;
      return = false;
      null = false;
      vars = T.Vars.empty;
      codes = T.Codes.comp x.codes;
    }

  let union x y =
    {
      call = x.call || y.call;
      return = x.return || y.return;
      null = x.null || y.null;
      vars = T.Vars.union x.vars y.vars;
      codes = T.Codes.union x.codes y.codes;
    }

  let inter x y =
    {
      call = x.call && y.call;
      return = x.return && y.return;
      null = x.null && y.null;
      vars = T.Vars.inter x.vars y.vars;
      codes = T.Codes.inter x.codes y.codes;
    }

  let diff x y =
    {
      call = x.call && not y.call;
      return = x.return && not y.return;
      null = x.null && not y.null;
      vars = T.Vars.diff x.vars y.vars;
      codes = T.Codes.diff x.codes y.codes;
    }
end

let refine xs =
  let module Refine = Refine.Make(Lits) in
  let f = Refine.refine xs in
  Seq.cons (Lits.comp (Refine.default f)) (Refine.partitions f)

module R = Re.Concrete(Lits)
module R_to = Balanced_binary_tree.Map.Size(R)

module Production = struct
  type t = {lhs: T.Var.t; rhs: R.t}
  [@@deriving eq, ord]

  let make lhs rhs = {lhs; rhs}
end
module Productions = Balanced_binary_tree.Set.Size(Production)
module M = Fa.Make(Productions)(Lits)
module Gen = M.Gen(T.State_index(R_to))

let output is_final p =
  if is_final 
  then Productions.singleton p
  else Productions.empty

let convert ~supply p =
  Gen.unfold ~supply ~merge:Lits.union (fun _ r ->
      let is_final = R.is_nullable r in
      (is_final, output is_final p, Seq.map (fun ls -> (ls, R.simplify @@ R.derivative ls r)) (refine @@ R.first r)))
    p.Production.rhs

module Acc' = Multimap.Make(T.Var_to)
module Acc = Acc'.L2(T.States)

let expand ~supply s' ps =
  let (_, t, s, f) = List.fold_left (fun (supply', t, s, f) p ->
      let supply, supply' = Supply.split2 supply' in
      let m = convert ~supply p in
      (supply', M.sum m t, Acc.add p.Production.lhs (M.start m) s, Acc.add_multiple p.Production.lhs (M.final m) f))
      (supply, M.empty, Acc.empty, Acc.empty)
      ps
  in
  M.{
    start = M.Start.Multiple (Acc.find_multiple s' s);
    final = Acc.find_multiple s' f;
    graph = M.skeleton (Seq.fold_left (fun t (q0, q1, ls) -> 
      T.Vars.fold (fun x t ->
        t
        |> M.link ~merge:Lits.union (T.States.singleton q0) (Acc.find_multiple x s) Lits.call
        |> M.link ~merge:Lits.union  (Acc.find_multiple x f) (T.States.singleton q1) Lits.return)
        t ls.Lits.vars)
    t (M.transitions t))
  }
    


(*let expand ~supply ps =*)




