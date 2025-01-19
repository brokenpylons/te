open Te_bot
open! Prelude
module T = Types

(*
   NOTE: The C_A and S_A transitions correspond to productions
*)

(*
   NOTE: There is a mapping between multiple production per nonterminal grammar and single production per nonterminal grammar.
   We basically do the conversion before.
*)

(*
   NOTE: Predict arises naturaly from LR automaton, the LHS of items is the predict symbol.
*)

module Lits: sig
  type vars = T.Vars.t
  type t =
    {
      eof: bool;
      call: T.Vars.t;
      return: T.Vars.t;
      scan: T.Vars.t;
      scan': bool;
      vars: T.Vars.t;
      codes: T.Codes.t;
    }

  include Re.LITS with type t := t
  include Refine.PARTITION with type t := t
  val comp: t -> t
  val is_eof: t -> bool
  val is_return: t -> bool
  val is_call: t -> bool
  val is_scan: t -> bool
  val is_scan': t -> bool
  val is_nullable: (T.Var.t -> bool) -> t -> bool

  val eof: t
  val empty: t
  val call: T.Vars.t -> t
  val return: T.Vars.t -> t
  val scan: T.Vars.t -> t
  val scan': t

  val of_vars: T.Vars.t -> t
  val to_vars: t -> T.Vars.t

  val of_codes: T.Codes.t -> t
  val to_codes: t -> T.Codes.t

  val extract_eof: t -> t
  val extract_vars: t -> t
  val extract_codes: t -> t

  val of_symbols: T.Symbols.t -> t

  val is_delegate: t -> bool
  val delegate: t -> T.Vars.t
end = struct
  type vars = T.Vars.t
  type t =
    {
      eof: bool;
      call: T.Vars.t;
      return: T.Vars.t;
      scan: T.Vars.t;
      scan': bool;
      vars: T.Vars.t;
      codes: T.Codes.t
    }
  [@@deriving eq, ord]

  let pp ppf x =
    Fmt.pf ppf "@[%a%a%a%a%a%a%a@]"
      (pp_if x.eof Fmt.string) T.eof_string
      (pp_if (not @@ T.Vars.is_empty x.call) (fun ppf -> Fmt.pf ppf "𝓒 %a" T.Vars.pp)) x.call
      (pp_if (not @@ T.Vars.is_empty x.return) (fun ppf -> Fmt.pf ppf "𝓡 %a" T.Vars.pp)) x.return
      (pp_if (not @@ T.Vars.is_empty x.scan) (fun ppf -> Fmt.pf ppf "𝓢 %a" T.Vars.pp)) x.scan
      (pp_if x.scan' Fmt.string) "𝓢 "
      (pp_if (not @@ T.Vars.is_empty x.vars) T.Vars.pp) x.vars
      (pp_if (not @@ T.Codes.is_empty x.codes) T.Codes.pp) x.codes

  let subset x y =
    Bool.imp x.eof y.eof &&
    T.Vars.subset x.call y.call &&
    T.Vars.subset x.return y.return &&
    T.Vars.subset x.scan y.scan &&
    Bool.imp x.scan' y.scan' &&
    T.Vars.subset x.vars y.vars &&
    T.Codes.subset x.codes y.codes

  let empty =
    {
      eof = false;
      call = T.Vars.empty;
      return = T.Vars.empty;
      scan = T.Vars.empty;
      scan' = false;
      vars = T.Vars.empty;
      codes = T.Codes.empty;
    }

  let is_empty x =
    not x.eof &&
    T.Vars.is_empty x.call &&
    T.Vars.is_empty x.return &&
    T.Vars.is_empty x.scan &&
    not x.scan' &&
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

  let is_scan' x =
    x.scan'

  let eof = {empty with eof = true}
  let call x = {empty with call = x}
  let return x = {empty with return = x}
  let scan x = {empty with scan = x}
  let scan' = {empty with scan' = true}

  let of_vars x = {empty with vars = x}
  let to_vars x = x.vars

  let of_codes x = {empty with codes = x}
  let to_codes x = x.codes

  let extract_eof x =
    if is_eof x then eof else empty

  let extract_vars x =
    (of_vars % to_vars) x

  let extract_codes x =
    (of_codes % to_codes) x

  let of_symbols (x: T.Symbols.t) =
    {
      eof = T.Symbols.is_eof x;
      call = T.Vars.empty;
      return = T.Vars.empty;
      scan = T.Vars.empty;
      scan' = false;
      codes = T.Symbols.to_codes x;
      vars = T.Symbols.to_vars x;
    }

  let comp x =
    {
      eof = true;
      call = T.Vars.empty;
      return = T.Vars.empty;
      scan = T.Vars.empty;
      scan' = false;
      vars = T.Vars.empty;
      codes = T.Codes.comp x.codes;
    }

  let union x y =
    {
      eof = x.eof || y.eof;
      call = T.Vars.union x.call y.call;
      return = T.Vars.union x.return y.return;
      scan = T.Vars.union x.scan y.scan;
      scan' = x.scan' || y.scan';
      vars = T.Vars.union x.vars y.vars;
      codes = T.Codes.union x.codes y.codes;
    }

  let inter x y =
    {
      eof = x.eof && y.eof;
      call = T.Vars.inter x.call y.call;
      return = T.Vars.inter x.return y.return;
      scan = T.Vars.inter x.scan y.scan;
      scan' = x.scan' && y.scan';
      vars = T.Vars.inter x.vars y.vars;
      codes = T.Codes.inter x.codes y.codes;
    }

  let diff x y =
    {
      eof = not (Bool.imp x.eof y.eof);
      call = T.Vars.diff x.call y.call;
      return = T.Vars.diff x.return y.return;
      scan = T.Vars.diff x.scan y.scan;
      scan' = not (Bool.imp x.scan' y.scan');
      vars = T.Vars.diff x.vars y.vars;
      codes = T.Codes.diff x.codes y.codes;
    }

  let is_nullable f x =
    T.Vars.exists f x.vars

  let is_delegate x =
    is_call x || is_scan x || is_scan' x

  let delegate x =
    T.Vars.union x.call x.scan
end

module type SET = sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool

  val empty: t
  val is_empty: t -> bool
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
  type t =
    {
      eof: Values.t;
      call: VM.t;
      return: VM.t;
      scan: VM.t;
      scan': Values.t;
      vars: VM.t;
      codes: CM.t
    }
  [@@deriving eq, ord]

  let pp pp_s ppf x =
    Fmt.pf ppf "@[@[%a@]@,@[%a@]@,@[%a@]@,@[%a@]@,@[%a@]@,@[%a@]@,@[%a@]@]"
      (pp_if (not @@ Values.is_empty x.eof) (fun ppf -> Fmt.pf ppf "%s %a" T.eof_string pp_s)) x.eof
      (T.Var_to.pp pp_s) x.call
      (T.Var_to.pp pp_s) x.return
      (T.Var_to.pp pp_s) x.scan
      (pp_if (not @@ Values.is_empty x.scan') (fun ppf -> Fmt.pf ppf "%s %a" T.delegate_string pp_s)) x.scan'
      (T.Var_to.pp pp_s) x.vars
      (Fmt.seq (Fmt.pair T.Codes.pp pp_s)) (CM.partitions x.codes)

  let empty =
    {
      eof = Values.empty;
      call = VM.empty;
      return = VM.empty;
      scan = VM.empty;
      scan' = Values.empty;
      vars = VM.empty;
      codes = CM.empty;
    }

  let add_multiple k vs t =
    {
      eof = if Lits.is_eof k then Values.union vs t.eof else t.eof;
      call = VM.add k.call vs t.call;
      return = VM.add k.return vs t.return;
      scan = VM.add k.scan vs t.scan;
      scan' = if Lits.is_scan' k then Values.union vs t.scan' else t.scan';
      vars = VM.add k.vars vs t.vars;
      codes = CM.add k.codes vs t.codes;
    }

  let singleton_multiple k vs = add_multiple k vs empty

  let find_multiple k t =
    let (<|>) = Values.union in
    (if Lits.is_eof k then t.eof else Values.empty) <|>
    VM.find k.call t.call <|>
    VM.find k.return t.return <|>
    VM.find k.scan t.scan <|>
    (if Lits.is_scan' k then t.scan' else Values.empty) <|>
    VM.find k.vars t.vars <|>
    CM.find k.codes t.codes

  let find_multiple_or_empty k t =
      try find_multiple k t
      with Not_found -> Values.empty

  let union x y =
    {
      eof = Values.union x.eof y.eof;
      call = VM.union x.call y.call;
      return = VM.union x.return y.return;
      scan = VM.union x.scan y.scan;
      scan' = Values.union x.scan' y.scan';
      vars = VM.union x.vars y.vars;
      codes = CM.union x.codes y.codes;
    }

  let (<|>) = union

  let add_seq_multiple s t =
    Seq.fold_left (fun t (k, v) -> add_multiple k v t) t s

  let of_seq_multiple s =
    add_seq_multiple s empty
end

module Enhanced_var: sig
  type t = T.State.t * T.Var.t
  val compare: t -> t -> int
  val strip: t -> T.Var.t
  val pp: t Fmt.t
end = struct
  type t = T.State.t * T.Var.t
  [@@deriving ord]

  let strip (_, var) = var

  let pp = Fmt.pair T.State.pp T.Var.pp
end
module Enhanced_vars = struct
  include Multimap.Make3(T.State_to)(T.Vars)

  let singleton (evar: Enhanced_var.t) =
    uncurry singleton evar
end

module Numbered_enhanced_var: sig
  type t = int * Enhanced_var.t
  val compare: t -> t -> int
  val pp: t Fmt.t
end = struct
  type t = int * Enhanced_var.t
  [@@deriving ord]

  let pp = Fmt.pair Fmt.int Enhanced_var.pp
end
module Number_to = Balanced_binary_tree.Map.Size(Int)
module Numbered_enhanced_var_to = Balanced_binary_tree.Map.Size(Numbered_enhanced_var)


module Enhanced_lits: sig
  include Re.LITS with type t = Lits.t T.State_to.t
  type vars = Enhanced_vars.t
  val make: T.State.t -> Lits.t -> t
  val strip: t -> Lits.t
  val filter: (T.State.t -> Lits.t -> bool) -> t -> t
  val fold: (T.State.t -> Lits.t -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  val restrict: (T.State.t -> Lits.t -> Lits.t) -> t -> t

  val empty: t
  val is_empty: t -> bool
  val union: t -> t -> t
  val diff: t -> t -> t
  val inter: t -> t -> t
  val to_seq: t -> (T.State.t * Lits.t) Seq.t
  val to_vars: t -> Enhanced_vars.t
  val of_vars: Enhanced_vars.t -> t

  val extract_eof: t -> t
  val extract_vars: t -> t
  val extract_codes: t -> t

  val is_delegate: t -> bool
  val delegate: t -> Enhanced_vars.t
end = struct
  module M = Multimap.Make2(T.State_to)(Lits)

  type vars = Enhanced_vars.t
  type t = M.t
  [@@deriving eq, ord]

  let pp =
    T.State_to.pp Lits.pp

  let to_vars x =
    M.fold_multiple (fun q lts -> Enhanced_vars.add_multiple q (Lits.to_vars lts)) Enhanced_vars.empty x

  let of_vars x =
    Enhanced_vars.fold_multiple (fun q vs -> M.add_multiple q (Lits.of_vars vs)) M.empty x

  let restrict f x =
    M.fold_multiple (fun q lts acc ->
        let lts' = f q lts in
        if Lits.is_empty lts'
        then acc
        else M.add_multiple q lts' acc)
      M.empty x

  let extract_eof x =
    restrict (fun _ -> Lits.extract_eof) x

  let extract_vars x =
    restrict (fun _ -> Lits.extract_vars) x

  let extract_codes x =
    restrict (fun _ -> Lits.extract_codes) x

  let subset x y =
    T.State_to.subset Lits.equal x y

  let strip t =
    M.fold_multiple (fun _ lts acc -> Lits.union lts acc) Lits.empty t

  let is_delegate x =
    M.fold_multiple (fun _ lts acc -> acc || Lits.is_delegate lts) false x

  let delegate x =
    M.fold_multiple (fun q lts acc ->
        let lts' = Lits.delegate lts in
        if T.Vars.is_empty lts'
        then acc
        else Enhanced_vars.add_multiple q lts' acc)
      Enhanced_vars.empty x

  let make = M.singleton_multiple
  let empty = M.empty
  let is_empty = M.is_empty
  let union = M.union
  let diff = M.diff
  let inter = M.inter
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
    T.State_to.fold (fun q lts -> M.add_multiple q (LM.singleton_multiple lts vs)) x k

  let singleton_multiple k vs = add_multiple k vs empty

  let union = M.union
  let (<|>) = M.union

  let find_multiple k x =
    T.State_to.fold (fun q lts ->
        S.union (LM.find_multiple lts (M.find_multiple q x)))
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

module Rhs = struct
  include Re.Abstract
  include Re.Concrete(Lits)
end

module Preitem = struct
  type t = {number: int; lhs: T.Labeled_var.t; rhs: Rhs.t; is_kernel: bool; is_reduce: bool; distance: Size.t}
  [@@deriving eq, ord]

  let pp ppf {number; lhs; rhs; is_kernel; is_reduce; distance} =
    Fmt.pf ppf "@[%i %a ::= %a %a %a %a@]"
      number
      T.Labeled_var.pp lhs
      Rhs.pp rhs
      (pp_if is_kernel Fmt.string) "K"
      (pp_if is_reduce Fmt.string) "R"
      Size.pp distance
end
module Preitem_to = Balanced_binary_tree.Map.Size(Preitem)

module Item = struct
  type t = {number: int; state: T.State.t; lhs: T.Labeled_var.t; rhs: Rhs.t; is_kernel: bool; is_reduce: bool; distance: Size.t}
  [@@deriving eq, ord]

  let pp ppf {number; state; lhs; rhs; is_kernel; is_reduce; distance} =
    Fmt.pf ppf "@[%i %a %a ::= %a %a %a %a@]"
      number
      T.State.pp state
      T.Labeled_var.pp lhs
      Rhs.pp rhs
      (pp_if is_kernel Fmt.string) "K"
      (pp_if is_reduce Fmt.string) "R"
      Size.pp distance
end

module Items = struct
  include Balanced_binary_tree.Set.Size(Item)
  let pp = pp Item.pp

  let is_reduce its =
    exists (fun it -> it.is_reduce) its
end

module Production = struct
  type t = {lhs: T.Labeled_var.t; rhs: Rhs.t}
  [@@deriving eq, ord]

  let pp ppf {lhs; rhs; _} =
    Fmt.pf ppf "@[%a ::= %a@]"
      T.Labeled_var.pp lhs
      Rhs.pp rhs
end

module Numbered_production = struct
  type t = {number: int; lhs: T.Labeled_var.t; rhs: Rhs.t}
  [@@deriving eq, ord]

  let pp ppf {lhs; rhs; _} =
    Fmt.pf ppf "@[%a ::= %a@]"
      T.Labeled_var.pp lhs
      Rhs.pp rhs
end
module Numbered_productions = Balanced_binary_tree.Set.Size(Numbered_production)
module Production_index = Multimap.Make3(T.Var_to)(Numbered_productions)

module Var_state = struct
  type t = T.Var.t * T.State.t
  [@@deriving eq, ord]

  let pp = Fmt.parens (Fmt.pair ~sep:(Fmt.const Fmt.string ":") T.Var.pp T.State.pp)
end
module Var_state_to = struct
  include Balanced_binary_tree.Map.Size(Var_state)

  let pp pp_p = pp Var_state.pp pp_p
end

(* Group by state *)
module Noncanonical_items = struct
  include Multimap.Make3(T.State_to)(Items)

  let join nits =
    fold_multiple (fun _ its acc -> Items.union its acc) Items.empty nits

end

module A = struct
  include Fa.Make(T.State)(T.States)(T.State_graph)
end

module PA = struct
  include Fa.Make(T.State_pair)(T.State_pairs)(T.State_pair_graph)
end

module Enhanced_production = struct
  type t = {number: int; lhs: Enhanced_var.t; rhs: (PA.Start.single, Items.t, Enhanced_lits.t) PA.t}
end

module Actions_multimap = struct
  include Lits_multimap(T.Actions)
  let pp = pp T.Actions.pp
end

let refine xs =
  let module R = Refine.Set(Lits) in
  let f = R.refine xs in
  Seq.cons (Lits.comp (R.considered f)) (R.partitions f)

let index_productions ~supply ps =
  let supply = ref supply in
  Seq.fold_left (fun idx p ->
      let (number, supply') = Supply.get !supply in
      supply := supply';
      Production_index.add (T.Labeled_var.var p.Production.lhs) Numbered_production.{
          number;
          lhs = p.lhs;
          rhs = p.rhs;
        } idx)
    Production_index.empty
    ps

let construct ~supply start lexical prods =
  let module Gen = A.Gen(T.State_index(Preitem_to)) in
  Gen.unfold ~supply ~merge:Lits.union (fun q {number; lhs; rhs; is_kernel; is_reduce; distance} ->
      let fs = refine @@ Rhs.first rhs in
      let kernel = Seq.filter_map (fun lts ->
          let rhs' = Rhs.simplify @@ Rhs.derivative lts rhs in
          if Rhs.is_nothing rhs'
          then None
          else Some (lts, Preitem.{
              number;
              lhs;
              rhs = rhs';
              distance = if Rhs.is_infinite rhs' then Size.top else Size.succ distance;
              is_kernel = true;
              is_reduce = Rhs.is_nullable rhs';
            }))
          fs
      in
      let vars = Lits.to_vars @@ Seq.fold_left Lits.union Lits.empty fs in
      (* OVEREXPAND *)
      (*let vars = if not @@ T.Vars.is_empty (T.Vars.inter vars lexical)
        then T.Vars.union vars lexical
        else vars
        in*)
      let nonkernel =
        Seq.flat_map (fun var ->
            Seq.map (fun Numbered_production.{number; lhs; rhs} ->
                ((if T.Vars.mem var lexical then Lits.scan else Lits.call) (T.Vars.singleton var), Preitem.{
                     number;
                     lhs;
                     rhs;
                     distance = Size.zero;
                     is_kernel = false;
                     is_reduce = Rhs.is_nullable rhs;
                   }))
              (Numbered_productions.to_seq @@ Production_index.find_multiple var prods))
          (T.Vars.to_seq @@ vars)
      in
      Items.singleton @@ Item.{number; state = q; lhs; rhs; is_kernel; is_reduce; distance}, Seq.(kernel @ nonkernel))
    (let Numbered_production.{number; lhs; rhs} =
       Numbered_productions.the @@ Production_index.find_multiple start prods in
     Preitem.{
       number;
       lhs;
       rhs;
       distance = Size.zero;
       is_kernel = false;
       is_reduce = Rhs.is_nullable rhs;
     })

let lr_items qs a =
  T.States.to_seq qs
  |> Seq.map (fun q -> A.labels q a)
  |> Seq.fold_left Items.union Items.empty

let noncanonical_items qs a =
  T.States.to_seq qs
  |> Seq.map (fun q -> (q, A.labels q a))
  |> Noncanonical_items.of_seq_multiple

let closure a qs =
  let module C = Closure.Make(T.States) in
  C.closure (fun q -> A.goto q Lits.is_call a) qs

let lr ~supply a =
  let module Gen = A.Gen(T.State_index(T.States_to)) in
  Gen.unfold ~supply ~merge:Lits.union (fun _ from ->
      let module R = Refine.Map(Lits)(T.States) in
      let from = closure a from in
      let next =
        A.adjacent_multiple from a
        |> Seq.filter_map (fun (s, lts) -> if Lits.is_call lts then None else Some (lts, T.States.singleton s))
        |> R.refine
        |> R.partitions
      in
      (lr_items from a, next))
    (closure a @@ A.start_multiple a)

let noncanonical_subset ~supply a =
  let module Gen = A.Gen(T.Preserve_state_index) in
  Gen.unfold ~supply ~merge:Lits.union (fun _ from ->
      let module R = Refine.Map(Lits)(T.States) in
      let next =
        A.adjacent_multiple from a
        |> Seq.filter_map (fun (s, lts) -> if Lits.is_call lts then None else Some (lts, T.States.singleton s))
        |> R.refine
        |> R.partitions
      in
      (noncanonical_items from a, next))
    (closure a @@ A.start_multiple a)

let enhance sa a =
  let start = (A.start sa, A.start a) in
  PA.unfold (fun _ (sa_from, a_from) ->
      let next = Seq.product (A.adjacent sa_from sa) (A.adjacent a_from a)
                 |> Seq.filter_map (fun ((sa_to, sa_c), (a_to, a_c)) ->
                     let c = Lits.inter sa_c a_c in
                     if Lits.is_empty c
                     then None
                     else Some (Enhanced_lits.make sa_from c, (sa_to, a_to), (sa_to, a_to)))
      in
      let next' =
        A.adjacent a_from a
        |> Seq.filter_map (fun (s, lts) ->
            if Lits.is_call lts
            then Some (Enhanced_lits.make sa_from lts, (sa_from, s), (sa_from, s))
            else None)
      in
      A.labels a_from a, Seq.(next @ next'))
    start start

let extract a =
  PA.unfold (fun _ from ->
      let next =
        PA.adjacent from a
        |> Seq.filter_map (fun (s, lts) ->
            if Enhanced_lits.is_delegate lts
            then None
            else Some (lts, s, s))
      in
      (PA.labels from a, next))
    (PA.start a) (PA.start a)

let enhanced_production (s, _) q ea =
  Seq.map (fun Item.{number; lhs = (_, var); _}  ->
      Enhanced_production.{
        number;
        lhs = (s, var);
        rhs = extract @@ PA.tail q ea;
      })
    (Items.to_seq @@ PA.labels q ea)

let enhanced_productions ea =
  PA.transitions ea
  |> Seq.filter (fun (_, _, lts) -> Enhanced_lits.is_delegate lts)
  |> Seq.flat_map (fun (s, q, _) -> enhanced_production s q ea)
  |> Seq.append (enhanced_production (PA.start ea) (PA.start ea) ea)

module Bool_set = struct
  type t = bool
  [@@deriving eq, ord]

  let union = (||)
  let empty = false
  let is_empty = Fun.id
end

module Analysis = struct
  module NM = Enhanced_lits_multimap(Bool_set)
  module EM = Enhanced_lits_multimap(Enhanced_lits)

  module Nullable = struct

    let start _ label =
      let its = label in
      Items.is_reduce its

    let next self current _ adj =
      Seq.fold_left (fun acc (lts, visit) -> NM.find_multiple_or_empty lts self && visit () || acc)
        current adj

    let compute_single_source rhs self =
      PA.dijkstra start (next self) rhs

    let compute_multi_source rhs self =
      PA.digraph start (next self) rhs

    let per_state prods self =
      prods
      |> Seq.map (fun Enhanced_production.{number; lhs; rhs} -> ((number, lhs), compute_multi_source rhs self))
      |> Numbered_enhanced_var_to.of_seq

    let update prods self =
      Seq.fold_left (fun acc Enhanced_production.{lhs; rhs; _} ->
          NM.add_multiple (Enhanced_lits.of_vars @@ Enhanced_vars.singleton lhs)
            (compute_single_source rhs self (PA.start rhs)) acc)
        self prods

    let per_lits prods self =
      Fixedpoint.run ~eq:NM.equal (update prods) self
  end

  module First = struct

    let start _ _ =
      Enhanced_lits.empty

    let next nullable_per_lits seed self current _ adj =
      Seq.fold_left (fun acc (lts, visit) ->
          let (<|>) = Enhanced_lits.union in
          seed lts <|> EM.find_multiple_or_empty lts self <|> acc <|> if nullable_per_lits lts then visit () else Enhanced_lits.empty)
        current adj

    let compute_single_source rhs nullable_per_lits seed self =
      PA.dijkstra start (next nullable_per_lits seed self) rhs

    let compute_multi_source rhs nullable_per_lits seed self =
      PA.digraph start (next nullable_per_lits seed self) rhs

    let per_state prods nullable_per_lits seed self =
      prods
      |> Seq.map (fun Enhanced_production.{number; lhs; rhs} -> ((number, lhs), compute_multi_source rhs nullable_per_lits seed self))
      |> Numbered_enhanced_var_to.of_seq

    let update prods nullable_per_lits seed self =
      Seq.fold_left (fun acc Enhanced_production.{lhs; rhs; _} ->
          EM.add_multiple (Enhanced_lits.of_vars @@ Enhanced_vars.singleton lhs)
            (compute_single_source rhs nullable_per_lits seed self (PA.start rhs)) acc)
        self prods

    let per_lits prods nullable_per_lits seed self =
      Fixedpoint.run ~eq:EM.equal (update prods nullable_per_lits seed) self
  end

  module Follow = struct

    let compute number lhs nullable_per_state first_per_state self q =
      let (<|>) = Enhanced_lits.union in
      first_per_state (number, lhs) q <|> if nullable_per_state (number, lhs) q
      then EM.find_multiple_or_empty (Enhanced_lits.of_vars @@ Enhanced_vars.singleton lhs) self
      else Enhanced_lits.empty

    let per_state prods nullable_per_state first_per_state self =
      prods
      |> Seq.map (fun Enhanced_production.{number; lhs; _} -> ((number, lhs), compute number lhs nullable_per_state first_per_state self))
      |> Numbered_enhanced_var_to.of_seq

    let update prods nullable_per_state first_per_state self =
      Seq.fold_left (fun acc Enhanced_production.{number; lhs; rhs} ->
          Seq.fold_left (fun acc (_, q, lts) ->
              let lts = Enhanced_lits.extract_vars lts in
              if not (Enhanced_lits.is_empty lts)
              then EM.add_multiple lts (compute number lhs nullable_per_state first_per_state self q) acc
              else acc)
            acc (PA.transitions rhs))
        self prods

    let per_lits prods nullable_per_state first_per_state self =
      Fixedpoint.run ~eq:EM.equal (update prods nullable_per_state first_per_state) self
  end

  let first_seed lts =
    let (<|>) = Enhanced_lits.union in
    Enhanced_lits.extract_eof lts <|> Enhanced_lits.extract_vars lts <|> Enhanced_lits.extract_codes lts

  let wrap ~default x number q  =
    try Numbered_enhanced_var_to.find number x q with Not_found -> default

  let combine c f1 f2 lts q =
    c (f1 lts q) (f2 lts q)

  let combine' c f1 f2 lts =
    c (f1 lts) (f2 lts)

  module Pre = struct
    type t =
      {
        nullable_per_lits: NM.t;
        nullable_per_state: Numbered_enhanced_var.t -> PA.state -> bool;
        first_per_lits: EM.t;
        first_per_state: Numbered_enhanced_var.t -> PA.state -> Enhanced_lits.t;
        follow_per_lits: EM.t;
        follow_per_state: Numbered_enhanced_var.t -> PA.state -> Enhanced_lits.t;
      }

    let empty =
      {
        nullable_per_lits = NM.empty;
        nullable_per_state = (fun _ _ -> false);
        first_per_lits = EM.empty;
        first_per_state = (fun _ _ -> Enhanced_lits.empty);
        follow_per_lits = EM.empty;
        follow_per_state = (fun _ _ -> Enhanced_lits.empty);
      }

    let compute (prods: Enhanced_production.t Seq.t) previous =
      print_endline "NULLABLE";
      let nullable_per_lits = Nullable.per_lits prods previous.nullable_per_lits in
      print_endline "NULLABLE_PER_STATE";
      let nullable_per_state = wrap ~default:false @@ Nullable.per_state prods nullable_per_lits in

      print_endline "FIRST";
      let first_per_lits = First.per_lits prods
          (fun lts -> NM.find_multiple_or_empty lts nullable_per_lits)
          first_seed previous.first_per_lits
      in
      print_endline "FIRST_PER_STATE";
      let first_per_state = wrap ~default:Enhanced_lits.empty @@ First.per_state prods
          (fun lts -> NM.find_multiple_or_empty lts nullable_per_lits)
          first_seed first_per_lits
      in
      print_endline "FOLLOW";
      let follow_per_lits = Follow.per_lits prods
          nullable_per_state
          first_per_state
          previous.follow_per_lits
      in
      print_endline "FOLLOW_PER_STATE";
      let follow_per_state = wrap ~default:Enhanced_lits.empty @@ Follow.per_state prods
          nullable_per_state
          first_per_state
          follow_per_lits
      in
      {
        nullable_per_lits;
        nullable_per_state = combine (||) nullable_per_state previous.nullable_per_state;
        first_per_lits;
        first_per_state = combine Enhanced_lits.union first_per_state previous.first_per_state;
        follow_per_lits;
        follow_per_state = combine Enhanced_lits.union follow_per_state previous.follow_per_state;
      }
  end

  type t =
    {
      nullable_per_lits: Enhanced_lits.t -> bool;
      nullable_per_state: Numbered_enhanced_var.t -> PA.state -> bool;
      first_per_lits: Enhanced_lits.t -> Enhanced_lits.t;
      first_per_state: Numbered_enhanced_var.t -> PA.state -> Enhanced_lits.t;
      follow_per_lits: Enhanced_lits.t -> Enhanced_lits.t;
      follow_per_state: Numbered_enhanced_var.t -> PA.state -> Enhanced_lits.t;
    }

  let compute (pre: Pre.t) =
    {
      nullable_per_lits = (fun lts -> NM.find_multiple_or_empty lts pre.nullable_per_lits);
      nullable_per_state = pre.nullable_per_state;
      first_per_lits = combine' Enhanced_lits.union first_seed (fun lts -> EM.find_multiple_or_empty lts pre.first_per_lits);
      first_per_state = pre.first_per_state;
      follow_per_lits = (fun lts -> EM.find_multiple_or_empty lts pre.follow_per_lits);
      follow_per_state = pre.follow_per_state;
    }
end

let index_enhanced_productions eprods =
  Seq.fold_left (fun acc Enhanced_production.{number; lhs; rhs} ->
      Number_to.add number ((lhs, rhs) :: try Number_to.find number acc with Not_found -> []) acc)
    Number_to.empty eprods

module Lookback = struct
  type t = Enhanced_production.t list Number_to.t

  let find number s q lookback =
    Number_to.find number lookback
    |> List.filter_map (fun (lhs, rhs) ->
        if PA.state_mem (s, q) rhs
        then Some (lhs: Enhanced_var.t)
        else None)
end

module Lookahead = struct
  type t =
    {
      right_nulled: bool;
      shift_lookahead: Enhanced_lits.t;
      reduce_lookahead: Enhanced_lits.t;
      code_lookahead: Enhanced_lits.t;
      lexical_lookahead: Enhanced_lits.t;
    }

  let compute analysis lookback lexical number s q =
    let lhss = Lookback.find number s q lookback in
    let right_nulled =
      lhss
      |> List.exists (fun lhs -> analysis.Analysis.nullable_per_state (number, lhs) (s, q))
    in
    let shift_lookahead =
      lhss
      |> List.map (fun lhs -> analysis.Analysis.follow_per_state (number, lhs) (s, q))
      |> List.fold_left Enhanced_lits.union Enhanced_lits.empty
    in
    let reduce_lookahead =
      lhss
      |> List.filter_map (fun lhs ->
          if analysis.Analysis.nullable_per_state (number, lhs) (s, q)
          then Some (analysis.Analysis.follow_per_lits (Enhanced_lits.of_vars @@ Enhanced_vars.singleton lhs))
          else None)
      |> List.fold_left Enhanced_lits.union Enhanced_lits.empty
    in
    let lexical_lookahead =
      let lexical_lits = Lits.of_vars lexical in
      shift_lookahead
      |> Enhanced_lits.restrict (fun _ -> Lits.inter lexical_lits)
    in
    let code_lookahead =
      Enhanced_lits.diff shift_lookahead (analysis.Analysis.first_per_lits lexical_lookahead)
      |> Enhanced_lits.restrict (fun _ lts -> Lits.union
                                    (Lits.extract_codes lts)
                                    (Lits.extract_eof lts))
    in
    {right_nulled; shift_lookahead; reduce_lookahead; lexical_lookahead; code_lookahead}
end

let add_backlinks lookahead' lookback a =
  let (let*) = Seq.bind in
  A.extend ~merge:Lits.union (fun s its ->
      let* Item.{number; state; _} = Items.to_seq its in
      let* () = Seq.guard (lookahead' number s state).Lookahead.right_nulled in
      let* (q, var) = List.to_seq @@ Lookback.find number s state lookback in
      let* t = T.States.to_seq @@ A.goto q (fun lts -> T.Vars.mem var (Lits.to_vars lts)) a in
      Seq.return (s, t, Lits.return (T.Vars.singleton var)))
    a

let erase_scan a =
  A.map_lits (fun lts -> if Lits.is_scan lts then Lits.scan' else lts) a

let noncanonical lexical lookahead' a =
  let (let*) = Seq.bind in
  A.extend ~merge:Lits.union (fun s its ->
      let* {number; lhs = (_, var); state; _} = Items.to_seq its in
      let* () = Seq.guard (not @@ T.Vars.mem var lexical) in
      let* (q, _) =
        (lookahead' number s state).Lookahead.lexical_lookahead
        |> Enhanced_lits.to_seq
      in
      let* (t, lts') = A.adjacent q a in

      let* () = Seq.guard (Lits.is_scan' lts') in
      Seq.return (s, t, Lits.scan'))
    a

let return lexical eprods =
  eprods
  |> Seq.filter_map (fun Enhanced_production.{lhs = (_, var); rhs; _} ->
      if T.Vars.mem var lexical
      then None
      else Some rhs)
  |> Seq.fold_left (PA.merge ~merge_labels:Items.union ~merge_lits:Enhanced_lits.union) PA.empty
  |> PA.rev

let resolve_tail tail =
  tail
  |> Rhs.to_seq ~cmp:T.Var.compare (T.Vars.to_seq % Lits.to_vars)
  |> Seq.map List.of_seq
  |> List.of_seq

let shift lexical lookahead' s' a =
  let (let*) = Seq.bind in
  let* (s, its) = Noncanonical_items.to_seq_multiple (A.labels s' a) in
  let* {number; lhs = (_, var); state; _} = Items.to_seq its in
  let* () = Seq.guard (not @@ T.Vars.mem var lexical) in
  Seq.return
    (Enhanced_lits.strip (lookahead' number s state).Lookahead.lexical_lookahead,
     T.Actions.shift)

let load lexical lookahead' s' a =
  let (let*) = Seq.bind in
  let* (s, its) = Noncanonical_items.to_seq_multiple (A.labels s' a) in
  let* {number; lhs = (_, var); state; _} = Items.to_seq its in
  let* () = Seq.guard (not @@ T.Vars.mem var lexical) in
  Seq.return
    (Enhanced_lits.strip (lookahead' number s state).Lookahead.code_lookahead,
     T.Actions.load)

let matches lexical lookahead' s' a =
  let (let*) = Seq.bind in
  let* (s, its) = Noncanonical_items.to_seq_multiple (A.labels s' a) in
  let* {number; lhs = (label, var); state; is_kernel; is_reduce; _} = Items.to_seq its in
  let* () = Seq.guard (T.Vars.mem var lexical && is_kernel && is_reduce) in
  Seq.return
    (Enhanced_lits.strip @@ (lookahead' number s state).Lookahead.reduce_lookahead,
     T.Actions.matches (T.Labeled_vars.singleton (label, var)))

let predictions lexical lookahead' s' a =
  let (let*) = Seq.bind in
  let* (s, its) = Noncanonical_items.to_seq_multiple (A.labels s' a) in
  let* {number; lhs = (_, var); state; _} = Items.to_seq its in
  let* () = Seq.guard (T.Vars.mem var lexical) in
  Seq.return
    (Enhanced_lits.strip @@ (lookahead' number s state).Lookahead.shift_lookahead,
     T.Actions.predictions (T.Vars.singleton var))

let null lexical lookahead' s' a =
  let (let*) = Seq.bind in
  let* (s, its) = Noncanonical_items.to_seq_multiple (A.labels s' a) in
  let* {number; lhs = (label, var); rhs; state; is_kernel; is_reduce; _} = Items.to_seq its in
  let right_nulled = (lookahead' number s state).Lookahead.right_nulled in
  let* () = Seq.guard (not @@ T.Vars.mem var lexical && not @@ is_kernel && right_nulled) in
  Seq.return
    (Enhanced_lits.strip @@ (lookahead' number s state).Lookahead.reduce_lookahead,
     T.Actions.null (T.Reductions.singleton
                       (T.Reduction.make
                          (label, var)
                          Null
                          (if not is_reduce
                           then Lists (resolve_tail rhs)
                           else Complete))))

let shift_null lexical lookahead' s' a =
  let (let*) = Seq.bind in
  let* (q, lts) = A.adjacent s' a in
  let* () = Seq.guard (Lits.is_scan' lts) in
  let* (s, its) = Noncanonical_items.to_seq_multiple (A.labels q a) in
  let* {number; lhs = (label, var); rhs; state; is_kernel; is_reduce; _} = Items.to_seq its in
  let right_nulled = (lookahead' number s state).Lookahead.right_nulled in
  let* () = Seq.guard (T.Vars.mem var lexical && not @@ is_kernel && right_nulled) in
  Seq.return
    (Enhanced_lits.strip @@ (lookahead' number s state).Lookahead.reduce_lookahead,
     T.Actions.null (T.Reductions.singleton
                       (T.Reduction.make (label, var)
                          Null
                          (if not is_reduce
                           then Lists (resolve_tail rhs)
                           else Complete))))

let select_strategy distance s' q =
  match Size.to_int @@ distance with
  | Some d -> T.Reduction.Strategy.Fixed d
  | None -> T.Reduction.Strategy.Scan (s', q)

let reduce lexical lookahead' s' a =
  let (let*) = Seq.bind in
  let* (s, its) = Noncanonical_items.to_seq_multiple (A.labels s' a) in
  let* {number; lhs = (label, var); rhs; state; is_kernel; is_reduce; distance} = Items.to_seq its in
  let right_nulled = (lookahead' number s state).Lookahead.right_nulled in
  let* () = Seq.guard (not @@ T.Vars.mem var lexical && is_kernel && right_nulled) in
  Seq.return
    (Enhanced_lits.strip @@ (lookahead' number s state).Lookahead.reduce_lookahead,
     T.Actions.reduce (T.Reductions.singleton
                         (T.Reduction.make (label, var)
                            (select_strategy distance s' state)
                            (if not is_reduce
                             then Lists (resolve_tail rhs)
                             else Complete))))

let actions lexical lookahead' s a =
  let fs = List.to_seq @@ [shift; load; matches; predictions; null; shift_null; reduce] in
  Seq.flat_map (fun f -> f lexical lookahead' s a) fs

(* FOR DEBUGGING *)
let with_lookahead lookahead' a =
  A.map_labels (fun s its ->
      Seq.map (fun Item.{number; lhs; state; _} ->
          (lhs, (lookahead' number s state).Lookahead.code_lookahead))
        (Items.to_seq its))
    a

let with_nullable lookahead' a =
  A.map_labels (fun s its ->
      Seq.map (fun Item.{number; lhs; state; _} ->
          (lhs, (lookahead' number s state).Lookahead.right_nulled))
        (Items.to_seq its))
    a

let actions' lexical lookahead' s a =
  actions lexical lookahead' s a
  |> Seq.filter (fun (lts, _) -> not @@ Lits.is_empty lts)
  |> Seq.map (fun (lts, x) -> Actions_multimap.singleton_multiple lts x)
  |> Seq.fold_left Actions_multimap.union Actions_multimap.empty


let with_actions lexical lookahead' a =
  A.map_labels (fun s _ ->
      actions' lexical lookahead' s a)
    a

let to_dot a = A.to_dot ~string_of_labels:(Fmt.to_to_string (Actions_multimap.pp)) ~string_of_lits:(Fmt.to_to_string Lits.pp) a

let to_dot' a = A.to_dot ~string_of_labels:(Fmt.to_to_string (Fmt.seq @@ Fmt.parens (Fmt.pair ~sep:(Fmt.const Fmt.string ": ") T.Labeled_var.pp Fmt.bool))) ~string_of_lits:(Fmt.to_to_string Lits.pp) a

let to_dot'' a = A.to_dot ~string_of_labels:(Fmt.to_to_string Items.pp) ~string_of_lits:(Fmt.to_to_string Lits.pp) a

let to_dot''' a = PA.to_dot ~string_of_labels:(Fmt.to_to_string Items.pp) ~string_of_lits:(Fmt.to_to_string Enhanced_lits.pp) a

let to_dot''''' a = A.to_dot ~string_of_labels:(Fmt.to_to_string (Fmt.seq @@ Fmt.parens (Fmt.pair ~sep:(Fmt.const Fmt.string ": ") T.Labeled_var.pp Enhanced_lits.pp))) ~string_of_lits:(Fmt.to_to_string Lits.pp) a

let print_productions prods =
  Seq.iter (fun prod ->
      Fmt.pr "@[@[%a@] ::= @[%s@]@]@." Enhanced_var.pp prod.Enhanced_production.lhs (Dot.string_of_graph @@ to_dot''' (prod.Enhanced_production.rhs)))
    prods

let build syntactic lexical _longest_match start prods  =
  assert (T.Vars.disjoint syntactic lexical);

  print_endline "CONSTRUCT";
  let iprods = index_productions ~supply:(T.State.fresh_supply ()) prods in
  let c = erase_scan @@ construct ~supply:(T.State.fresh_supply ()) start lexical iprods in

  print_endline "LR";
  let lr_supply1, lr_supply2 = Supply.split2 @@ T.State.fresh_supply () in
  let p =
    lr ~supply:lr_supply1 c
  in

  print_endline "ENHANCE";
  let e = enhance p c in
  let eprods1 = enhanced_productions e in

  print_endline "ANA";
  let pre_analysis = Analysis.Pre.compute eprods1 Analysis.Pre.empty in
  let analysis = Analysis.compute pre_analysis in

  (*Fmt.pr "C %s@,"  (Dot.string_of_graph (to_dot'' c));
  Fmt.pr "E %s@,"  (Dot.string_of_graph (to_dot''' e));*)

  print_endline "LOOK";

  let ieprods1 = index_enhanced_productions eprods1 in
  let lookahead' = Lookahead.compute analysis ieprods1 lexical in

  print_endline "NC";
  let nc =
    noncanonical lexical lookahead' p
    |> noncanonical_subset ~supply:lr_supply2
  in

  let back = return lexical eprods1 in

  (*print_productions eprods1;*)

  print_endline "END";
  (*Fmt.pr "NL %s@,"  (Dot.string_of_graph (to_dot' (with_nullable lookahead' (A.map_labels (fun _ -> Noncanonical_items.join) nc))));

  Fmt.pr "LK %s@,"  (Dot.string_of_graph (to_dot''''' (with_lookahead lookahead' (A.map_labels (fun _ -> Noncanonical_items.join) nc))));

  Fmt.pr "ITMS %s@,"  (Dot.string_of_graph (to_dot'' (A.map_labels (fun _ -> Noncanonical_items.join) nc)));

  Fmt.pr "%s@,"  (Dot.string_of_graph (to_dot (with_actions lexical lookahead' nc)));*)


  (lookahead', nc, back)

