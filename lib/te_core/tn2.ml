open Te_bot
open! Prelude
module T = Types

module Lits: sig
  type vars = T.Vars.t
  type t =
    {
      eof: bool;
      call: T.Vars.t;
      return: T.Vars.t;
      scan: T.Vars.t;
      vars: T.Vars.t;
      codes: T.Codes.t
    }

  include Re.LITS with type t := t
  include Refine.PARTITION with type t := t
  val comp: t -> t
  val is_eof: t -> bool
  val is_return: t -> bool
  val is_call: t -> bool
  val is_scan: t -> bool
  val is_nullable: (T.Var.t -> bool) -> t -> bool

  val eof: t
  val empty: t
  val call: T.Vars.t -> t
  val return: T.Vars.t -> t
  val scan: T.Vars.t -> t

  val of_vars: T.Vars.t -> t
  val to_vars: t -> T.Vars.t

  val of_codes: T.Codes.t -> t
  val to_codes: t -> T.Codes.t

  val of_symbols: T.Symbols.t -> t
end = struct
  type vars = T.Vars.t
  type t =
    {
      eof: bool;
      call: T.Vars.t;
      return: T.Vars.t;
      scan: T.Vars.t;
      vars: T.Vars.t;
      codes: T.Codes.t
    }
  [@@deriving eq, ord]

  let pp ppf x =
    Fmt.pf ppf "@[%a%a%a%a%a%a@]"
      (pp_if x.eof Fmt.string) T.eof_string
      (pp_if (not @@ T.Vars.is_empty x.scan) (fun ppf -> Fmt.pf ppf "𝓢 %a" T.Vars.pp)) x.scan
      (pp_if (not @@ T.Vars.is_empty x.call) (fun ppf -> Fmt.pf ppf "𝓒 %a" T.Vars.pp)) x.call
      (pp_if (not @@ T.Vars.is_empty x.return) (fun ppf -> Fmt.pf ppf "𝓡 %a" T.Vars.pp)) x.return
      (pp_if (not @@ T.Vars.is_empty x.vars) T.Vars.pp) x.vars
      (pp_if (not @@ T.Codes.is_empty x.codes) T.Codes.pp) x.codes

  let subset x y =
    Bool.imp x.eof y.eof &&
    T.Vars.subset x.call y.call &&
    T.Vars.subset x.return y.return &&
    T.Vars.subset x.scan y.scan &&
    T.Vars.subset x.vars y.vars &&
    T.Codes.subset x.codes y.codes

  let empty =
    {
      eof = false;
      call = T.Vars.empty;
      return = T.Vars.empty;
      scan = T.Vars.empty;
      vars = T.Vars.empty;
      codes = T.Codes.empty;
    }

  let is_empty x =
    not x.eof &&
    T.Vars.is_empty x.call &&
    T.Vars.is_empty x.return &&
    T.Vars.is_empty x.scan &&
    T.Vars.is_empty x.vars &&
    T.Codes.is_empty x.codes

  let is_eof x =
    x.eof

  let is_return x =
    not (T.Vars.is_empty x.return)

  let is_call x =
    not (T.Vars.is_empty x.call)

  let is_scan x =
    not (T.Vars.is_empty x.scan)

  let eof = {empty with eof = true}
  let call x = {empty with call = x}
  let return x = {empty with return = x}
  let scan x = {empty with scan = x}

  let of_vars x = {empty with vars = x}
  let to_vars x = x.vars

  let of_codes x = {empty with codes = x}
  let to_codes x = x.codes

  let of_symbols (x: T.Symbols.t) =
    {
      eof = T.Symbols.is_eof x;
      scan = T.Vars.empty;
      call = T.Vars.empty;
      return = T.Vars.empty;
      codes = T.Symbols.to_codes x;
      vars = T.Symbols.to_vars x;
    }

  let comp x =
    {
      eof = true;
      call = T.Vars.empty;
      return = T.Vars.empty;
      scan = T.Vars.empty;
      vars = T.Vars.empty;
      codes = T.Codes.comp x.codes;
    }

  let union x y =
    {
      eof = x.eof || y.eof;
      call = T.Vars.union x.call y.call;
      return = T.Vars.union x.return y.return;
      scan = T.Vars.union x.scan y.scan;
      vars = T.Vars.union x.vars y.vars;
      codes = T.Codes.union x.codes y.codes;
    }

  let inter x y =
    {
      eof = x.eof && y.eof;
      call = T.Vars.inter x.call y.call;
      return = T.Vars.inter x.return y.return;
      scan = T.Vars.inter x.scan y.scan;
      vars = T.Vars.inter x.vars y.vars;
      codes = T.Codes.inter x.codes y.codes;
    }

  let diff x y =
    {
      eof = not (Bool.imp x.eof y.eof);
      call = T.Vars.diff x.call y.call;
      return = T.Vars.diff x.return y.return;
      scan = T.Vars.diff x.scan y.scan;
      vars = T.Vars.diff x.vars y.vars;
      codes = T.Codes.diff x.codes y.codes;
    }

  let is_nullable f x =
    T.Vars.exists f x.vars
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
  val pp: values Fmt.t -> t Fmt.t

  val empty: t
  val add_multiple: key -> values -> t -> t
  val singleton_multiple: key -> values -> t
  val union: t -> t -> t
  val (<|>): t -> t -> t
  val find_multiple: key -> t -> values
  val find_multiple_or_empty: key -> t -> values
  val of_seq_multiple: (key * values) Seq.t -> t
end

module Lits_multimap(Values: SET): MULTIMAP with type key = Lits.t and type values = Values.t = struct
  module VM = Relation.Make(T.Var_to)(T.Vars)(Values)
  module CM = Refine.Map(T.Codes)(Values)

  type key = Lits.t
  type values = Values.t
  type t = {eof: Values.t; call: VM.t; return: VM.t; vars: VM.t; codes: CM.t}
  [@@deriving eq, ord]

  let pp pp_s ppf x =
    Fmt.pf ppf "@[@[%a@]@,@[%a@]@,@[%a@]@,@[%a@]@,@[%a@]@]"
      pp_s x.eof
      (T.Var_to.pp pp_s) x.call
      (T.Var_to.pp pp_s) x.return
      (T.Var_to.pp pp_s) x.vars
      (Fmt.seq (Fmt.pair T.Codes.pp pp_s)) (CM.partitions x.codes)

  let empty =
    {
      eof = Values.empty;
      call = VM.empty;
      return = VM.empty;
      vars = VM.empty;
      codes = CM.empty;
    }

  let add_multiple k vs t =
    {
      eof = if Lits.is_eof k then Values.union vs t.eof else t.eof;
      call = VM.add k.call vs t.call;
      return = VM.add k.return vs t.return;
      vars = VM.add k.vars vs t.vars;
      codes = CM.add k.codes vs t.codes;
    }

  let singleton_multiple k vs = add_multiple k vs empty

  let find_multiple k t =
    let (<|>) = Values.union in
    ((if Lits.is_eof k then t.eof else Values.empty)
     <|> VM.find k.call t.call
     <|> VM.find k.return t.return
     <|> VM.find k.vars t.vars
     <|> CM.find k.codes t.codes)

  let find_multiple_or_empty k t =
      try find_multiple k t
      with Not_found -> Values.empty

  let union x y =
    {
      eof = Values.union x.eof y.eof;
      call = VM.union x.call y.call;
      return = VM.union x.return y.return;
      vars = VM.union x.vars y.vars;
      codes = CM.union x.codes y.codes;
    }

  let (<|>) = union

  let add_seq_multiple s t =
    Seq.fold_left (fun t (k, v) -> add_multiple k v t) t s

  let of_seq_multiple s =
    add_seq_multiple s empty
end

module Enhanced_vars = Multimap.Make2(T.State_to)(T.Vars)

module Enhanced_lits: sig
  include Re.LITS with type t = Lits.t T.State_to.t
  type vars = Enhanced_vars.t
  val singleton: T.State.t -> Lits.t -> t
  val strip: t -> Lits.t
  val filter: (T.State.t -> Lits.t -> bool) -> t -> t
  val fold: (T.State.t -> Lits.t -> 'acc -> 'acc) -> 'acc -> t -> 'acc

  val empty: t
  val union: t -> t -> t
  val to_seq: t -> (T.State.t * Lits.t) Seq.t
  val to_vars: t -> Enhanced_vars.t
  val of_vars: Enhanced_vars.t -> t
end = struct
  module M = Multimap.Make1(T.State_to)(Lits)

  type vars = Enhanced_vars.t
  type t = M.t
  [@@deriving eq, ord]

  let to_vars x =
    M.fold_multiple (fun q ls -> Enhanced_vars.add_multiple q (Lits.to_vars ls)) Enhanced_vars.empty x

  let of_vars x =
    Enhanced_vars.fold_multiple (fun q vs -> M.add_multiple q (Lits.of_vars vs)) M.empty x

  let pp =
    T.State_to.pp Lits.pp

  let subset x y =
    T.State_to.subset Lits.equal x y

  let strip t =
    M.fold_multiple (fun _ ls acc -> Lits.union ls acc) Lits.empty t

  let singleton = M.singleton_multiple
  let empty = M.empty
  let union = M.union
  let filter = M.filter_multiple
  let fold = M.fold_multiple
  let to_seq = M.to_seq_multiple
end

module Enhanced_lits_multimap(S: SET): sig 
  include MULTIMAP with type key = Enhanced_lits.t and type values = S.t
  val strip: t -> Lits_multimap(S).t
end = struct
  module LM = Lits_multimap(S)
  module M = Multimap.Make1(T.State_to)(LM)

  type t = M.t
  [@@deriving eq, ord]
  type key = Enhanced_lits.t
  type values = S.t

  let pp pp_s ppf x =
    Fmt.pf ppf "%a" (T.State_to.pp (LM.pp pp_s)) x

  let empty = M.empty

  let add_multiple k vs x =
    T.State_to.fold (fun q ls -> M.add_multiple q (LM.singleton_multiple ls vs)) x k

  let singleton_multiple k vs = add_multiple k vs empty

  let union = M.union
  let (<|>) = M.union

  let find_multiple k x =
    T.State_to.fold (fun q ls ->
        S.union (LM.find_multiple ls (M.find_multiple q x)))
      S.empty k

  let find_multiple_or_empty k t =
      try find_multiple k t
      with Not_found -> S.empty

  let strip x =
    T.State_to.fold (Fun.const LM.union) LM.empty x

  let add_seq_multiple s t =
    Seq.fold_left (fun t (k, v) -> add_multiple k v t) t s

  let of_seq_multiple s =
    add_seq_multiple s empty
end


module A = struct
  include Fa2.Make(T.State)(T.States)(T.State_graph)
end

module PA = struct
  include Fa2.Make(T.State_pair)(T.State_pairs)(T.State_pair_graph)
end

module Rhs = struct
  include Re.Abstract
  include Re.Concrete(Lits)
end

module Item = struct
  type t = {lhs: T.Labeled_var.t; rhs: T.State.t; is_kernel: bool; is_reduce: bool; distance: Size.t}
  [@@deriving eq, ord]

  let pp ppf {lhs; rhs; is_kernel; is_reduce; distance = _} =
    Fmt.pf ppf "@[%a ::= %a %a %a@]"
      T.Labeled_var.pp lhs
      T.State.pp rhs
      (pp_if is_kernel Fmt.string) "K"
      (pp_if is_reduce Fmt.string) "R"
end

module Preitem = struct
  type t = {lhs: T.Labeled_var.t; rhs: Rhs.t; is_kernel: bool; is_reduce: bool; distance: Size.t}
  [@@deriving eq, ord]
end
module Preitem_to = Balanced_binary_tree.Map.Size(Preitem)

module Production = struct
  type t = {lhs: T.Labeled_var.t; rhs: Rhs.t}
  [@@deriving eq, ord]

  let pp ppf {lhs; rhs; _} =
    Fmt.pf ppf "@[%a ::= %a@]"
      T.Labeled_var.pp lhs
      Rhs.pp rhs
end
module Productions = Balanced_binary_tree.Set.Size(Production)
module Production_index = Multimap.Make1(T.Var_to)(Productions)

module Items = struct
  include Balanced_binary_tree.Set.Size(Item)
  let pp = pp Item.pp
end

let refine xs =
  let module R = Refine.Set(Lits) in
  let f = R.refine xs in
  Seq.cons (Lits.comp (R.considered f)) (R.partitions f)

let construct ~supply start lexical prods =
  let module Gen = A.Gen(T.State_index(Preitem_to)) in
  Gen.unfold_multiple ~supply ~merge:Lits.union (fun q Preitem_to.{lhs; rhs; is_kernel; is_reduce; distance} ->
      let fs = refine @@ Rhs.first rhs in
      let kernel = Seq.filter_map (fun lts ->
          let rhs' = Rhs.simplify @@ Rhs.derivative lts rhs in
          if Rhs.is_nothing rhs
          then None
          else Some (lts, Preitem.{
              lhs;
              rhs = rhs';
              distance = if Rhs.is_infinite rhs' then Size.top else Size.succ distance;
              is_kernel = true;
              is_reduce = Rhs.is_nullable rhs';
            }))
          fs
      in
      let nonkernel =
        Seq.flat_map (fun var ->
            Seq.map (fun Production.{lhs; rhs} ->
                ((if T.Vars.mem var lexical then Lits.scan (T.Vars.singleton var) else Lits.call (T.Vars.singleton var)), Preitem.{
                     lhs;
                     rhs;
                     distance = Size.zero;
                     is_kernel = false;
                     is_reduce = Rhs.is_nullable rhs;
                   }))
              (Productions.to_seq @@ Production_index.find_multiple var prods))
          (T.Vars.to_seq @@ Lits.to_vars @@ Seq.fold_left Lits.union Lits.empty fs)
      in
      Item.{lhs; rhs = q; is_kernel; is_reduce; distance}, Seq.append kernel nonkernel)
    (List.of_seq @@ Seq.map (fun Production.{lhs; rhs} ->
         Preitem.{
           lhs;
           rhs;
           distance = Size.zero;
           is_kernel = false;
           is_reduce = Rhs.is_nullable rhs;
         })
        (Productions.to_seq @@ Production_index.find_multiple start prods))

let closure t qs =
  let module C = Closure.Make(T.States) in
  C.closure (fun q -> A.goto q Lits.is_call t) qs

let subset ~supply t =
  let module Gen = A.Gen(T.State_index(T.States_to)) in
  Gen.unfold ~supply ~merge:Lits.union (fun _ from ->
      let module R = Refine.Map(Lits)(T.States) in
      let from = closure t from in
      let next =
        A.adjacent_multiple from t
        |> Seq.filter_map (fun (s, ls) -> if Lits.is_call ls then None else Some (ls, T.States.singleton s))
        |> R.refine
        |> R.partitions
      in
      (from, next))
    (closure t @@ A.start_multiple t)

(*
let inter a a' =
  let start = (A.start a, A.start a') in
  PA.unfold (fun _ (from1, from2) ->
      let adj = Seq.product (A.adjacent from1 a) (A.adjacent from2 a')
                |> Seq.filter_map (fun ((to1, c1), (to2, c2)) ->
                    let c = Lits.inter c1 c2 in
                    if Lits.is_empty c (*|| T.Labeled_vars.disjoint (M.labels to1 m1).Labels.predictions (M.labels to2 m2).Labels.predictions*)
                    then None
                    else Some (c, (to1, to2), (to1, to2)))
      in
      (*let adj' =
        M.adjacent from2 m2
        |> Seq.filter_map (fun (s, ls) -> if Lits.is_call ls then Some (ls, (from1, s), (from1, s)) else None)
      in
      let (_, rhs) = Items.the (M.labels from2 m2) in
      (rhs.Item_rhs.tail, Seq.(adj @ adj')))*)
    start start)*)
