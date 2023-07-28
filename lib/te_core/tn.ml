open Te_bot
open! Prelude
module T = Types
module G = T.State_graph

let pp_if b pp ppf  =
  if b then Fmt.pf ppf "%a@ " pp else Fmt.nop ppf

module Lits: sig
  type vars = T.Vars.t
  type t =
    {
      eof: bool;
      call: T.Vars.t;
      return: T.Vars.t;
      scan: T.Vars.t;
      null: bool; 
      vars: T.Vars.t;
      codes: T.Codes.t
    }

  include Re.LITS with type t := t and type vars := vars
  include Refine.PARTITION with type t := t
  val comp: t -> t
  val is_return: t -> bool
  val is_call: t -> bool
  val is_scan: t -> bool
  val is_null: t -> bool
  val is_nullable: (T.Var.t -> bool) -> t -> bool

  val eof: t
  val empty: t
  val null: t
  val call: T.Vars.t -> t
  val return: T.Vars.t -> t
  val scan: T.Vars.t -> t
  val var: T.Var.t -> t
  val vars: T.Vars.t -> t
  val code: T.Code.t -> t
  val codes: T.Codes.t -> t
end = struct
  type vars = T.Vars.t
  type t = 
    {
      eof: bool;
      call: T.Vars.t;
      return: T.Vars.t;
      scan: T.Vars.t;
      null: bool; 
      vars: T.Vars.t;
      codes: T.Codes.t
    }
  [@@deriving eq, ord]

  let pp ppf x =
    Fmt.pf ppf "@[%a%a%a%a%a%a%a@]"
      (pp_if x.eof Fmt.string) "$"
      (pp_if (not @@ T.Vars.is_empty x.scan) (fun ppf -> Fmt.pf ppf "▾ %a" T.Vars.pp)) x.scan
      (pp_if (not @@ T.Vars.is_empty x.call) (fun ppf -> Fmt.pf ppf "◂ %a" T.Vars.pp)) x.call
      (pp_if x.null Fmt.string) "ε"
      (pp_if (not @@ T.Vars.is_empty x.vars) T.Vars.pp) x.vars
      (pp_if (not @@ T.Codes.is_empty x.codes) T.Codes.pp) x.codes
      (pp_if (not @@ T.Vars.is_empty x.return) (fun ppf -> Fmt.pf ppf "▸ %a" T.Vars.pp)) x.return

  let subset x y =
    Bool.imp x.eof y.eof &&
    T.Vars.subset x.call y.call &&
    T.Vars.subset x.return y.return &&
    T.Vars.subset x.scan y.scan &&
    Bool.imp x.null y.null &&
    T.Vars.subset x.vars y.vars &&
    T.Codes.subset x.codes y.codes

  let empty =
    {
      eof = false;
      call = T.Vars.empty;
      return = T.Vars.empty;
      scan = T.Vars.empty;
      null = false;
      vars = T.Vars.empty;
      codes = T.Codes.empty;
    }

  let is_empty x =
    not x.eof &&
    T.Vars.is_empty x.call &&
    T.Vars.is_empty x.return &&
    T.Vars.is_empty x.scan &&
    not x.null &&
    T.Vars.is_empty x.vars &&
    T.Codes.is_empty x.codes

  let is_return x =
    not (T.Vars.is_empty x.return)

  let is_call x =
    not (T.Vars.is_empty x.call)

  let is_scan x =
    not (T.Vars.is_empty x.scan)

  let is_null x =
    x.null

  let eof = {empty with eof = true}
  let call x = {empty with call = x}
  let return x = {empty with return = x}
  let scan x = {empty with scan = x}
  let null = {empty with null = true}
  let code x = {empty with codes = T.Codes.singleton x}
  let codes x = {empty with codes = x}
  let var x = {empty with vars = T.Vars.singleton x}
  let vars x = {empty with vars = x}

  let of_vars = vars
  let to_vars x = x.vars

  let comp x =
    {
      eof = true;
      call = T.Vars.empty;
      return = T.Vars.empty;
      scan = T.Vars.empty;
      null = false;
      vars = T.Vars.empty;
      codes = T.Codes.comp x.codes;
    }

  let union x y =
    {
      eof = x.eof || y.eof;
      call = T.Vars.union x.call y.call;
      return = T.Vars.union x.return y.return;
      scan = T.Vars.union x.scan y.scan;
      null = x.null || y.null;
      vars = T.Vars.union x.vars y.vars;
      codes = T.Codes.union x.codes y.codes;
    }

  let inter x y =
    {
      eof = x.eof && y.eof;
      call = T.Vars.inter x.call y.call;
      return = T.Vars.inter x.return y.return;
      scan = T.Vars.inter x.scan y.scan;
      null = x.null && y.null;
      vars = T.Vars.inter x.vars y.vars;
      codes = T.Codes.inter x.codes y.codes;
    }

  let diff x y =
    {
      eof = not (Bool.imp x.eof y.eof);
      call = T.Vars.diff x.call y.call;
      return = T.Vars.diff x.return y.return;
      scan = T.Vars.diff x.scan y.scan;
      null = not (Bool.imp x.null y.null);
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

module Lits_multimap(S: SET): MULTIMAP with type key = Lits.t and type values = S.t = struct
  module V = Multimap.Make1(T.Var_to)(S)
  module C = Refine.Map(T.Codes)(S)

  type key = Lits.t
  type values = S.t
  type t = {eof: S.t; call: V.t; return: V.t; null: S.t; vars: V.t; codes: C.t}
  [@@deriving eq, ord]

  let pp pp_s ppf x =
    Fmt.pf ppf "(@[<v>@[eof %a@]@ @[call %a@]@ @[ret %a@]@ @[null %a@]@ @[vars %a@]@ @[codes %a@]@])"
      pp_s x.eof
      (T.Var_to.pp pp_s) x.call
      (T.Var_to.pp pp_s) x.return
      pp_s x.null
      (T.Var_to.pp pp_s) x.vars
      (Fmt.seq (Fmt.pair T.Codes.pp pp_s)) (C.partitions x.codes)

  module VS = struct
    let add ks vs t =
      T.Vars.fold (fun k -> V.add_multiple k vs) t ks

    let find ks t =
      T.Vars.fold (fun k -> S.union @@ V.find_multiple k t) S.empty ks
  end

  let empty =
    {
      eof = S.empty;
      call = V.empty;
      return = V.empty;
      null = S.empty;
      vars = V.empty;
      codes = C.empty;
    }

  let add_multiple k vs t =
    {
      eof = if k.Lits.eof then S.union vs t.eof else t.eof;
      call = VS.add k.call vs t.call;
      return = VS.add k.return vs t.return;
      null = if k.null then S.union vs t.null else t.null;
      vars = VS.add k.vars vs t.vars;
      codes = C.add k.codes vs t.codes;
    }

  let singleton_multiple k vs = add_multiple k vs empty

  let find_multiple k t =
    let (<|>) = S.union in
    ((if k.Lits.eof then t.eof else S.empty)
     <|> VS.find k.call t.call 
     <|> VS.find k.return t.return 
     <|> (if k.null then t.null else S.empty)
     <|> VS.find k.vars t.vars
     <|> C.find k.codes t.codes)

  let find_multiple_or_empty k t =
      try find_multiple k t
      with Not_found -> S.empty

  let union x y =
    {
      eof = S.union x.eof y.eof;
      call = V.union x.call y.call;
      return = V.union x.return y.return;
      null = S.union x.null y.null;
      vars = V.union x.vars y.vars;
      codes = C.union x.codes y.codes;
    }

  let (<|>) = union

  let add_seq_multiple s t =
    Seq.fold_left (fun t (k, v) -> add_multiple k v t) t s

  let of_seq_multiple s =
    add_seq_multiple s empty
end

module R = Re.Abstract
module R' = Re.Porcelan(Re.Abstract)
module Rhs = Re.Concrete(T.Vars)(Lits)
module Rhs_to = Balanced_binary_tree.Map.Size(Rhs)

module Item_rhs = struct
  type t = {state: T.State.t; tail: Rhs.t; kernel: bool; dead: bool; reduce: bool}
  [@@deriving eq, ord]

  let pp ppf x =
    Fmt.pf ppf "(@[%a %a %a %a %a@])"
      T.State.pp x.state
      Rhs.pp x.tail
      (pp_if x.kernel Fmt.string) "K"
      (pp_if x.dead Fmt.string) "D"
      (pp_if x.reduce Fmt.string) "R"
end
module Item_rhss = Balanced_binary_tree.Set.Size(Item_rhs)

module Items = struct
  include Multimap.Make2(T.Labeled_var_to)(Item_rhss)

  let pp = T.Labeled_var_to.pp (Item_rhss.pp Item_rhs.pp)

  let matches x =
    T.Labeled_var_to.to_seq x
    |> Seq.flat_map (fun (lhs, rhss) ->
        Item_rhss.to_seq rhss
        |> Seq.filter_map (fun rhs -> if rhs.Item_rhs.reduce then Some lhs else None))
    |> T.Labeled_vars.of_seq

  let the = the
  let singleton = singleton
  let union x y = union x y
  let empty = empty
  let to_seq = to_seq
end
type items = Items.t

module Labels = Items

(*struct
  type t = Items.t

  let pp ppf x =
    Fmt.pf ppf "@[%a%a%a%a%a@]"
      (pp_if (not @@ R.is_nothing x.tail) (fun ppf -> Fmt.pf ppf "%a" Rhs.pp)) x.tail
      (pp_if (not @@ Items.is_empty x.items) (fun ppf -> Fmt.pf ppf "%a" Items.pp)) x.items
      (pp_if (not @@ T.Labeled_vars.is_empty x.started) (fun ppf -> Fmt.pf ppf "S: %a" T.Labeled_vars.pp)) x.started
      (pp_if (not @@ T.Labeled_vars.is_empty x.predictions) (fun ppf -> Fmt.pf ppf "P: %a" T.Labeled_vars.pp)) x.predictions
      (pp_if (not @@ T.Labeled_vars.is_empty x.matches) (fun ppf -> Fmt.pf ppf "M: %a" T.Labeled_vars.pp)) x.matches

  let empty =
    {
      items = Items.empty;
      tail = R.nothing;
      started = T.Labeled_vars.empty;
      predictions = T.Labeled_vars.empty;
      matches = T.Labeled_vars.empty;
    }

  let union x y =
    {
      items = Items.union x.items y.items;
      tail = R.union x.tail y.tail;
      started = T.Labeled_vars.union x.started y.started;
      predictions = T.Labeled_vars.union x.predictions y.predictions;
      matches = T.Labeled_vars.union x.matches y.matches;
    }
end*)

module Enhanced_vars = Multimap.Make2(T.State_to)(T.Vars)

module Enhanced_lits: sig
  include Re.LITS with type t = Lits.t T.State_to.t and type vars = Enhanced_vars.t
  val singleton: T.State.t -> Lits.t -> t
  val strip: t -> Lits.t
  val filter: (T.State.t -> Lits.t -> bool) -> t -> t
  val fold: (T.State.t -> Lits.t -> 'acc -> 'acc) -> 'acc -> t -> 'acc

  val empty: t
  val union: t -> t -> t
  val to_seq: t -> (T.State.t * Lits.t) Seq.t
end = struct
  module M = Multimap.Make1(T.State_to)(Lits)

  type vars = Enhanced_vars.t
  type t = M.t
  [@@deriving eq, ord]

  let to_vars x =
    M.fold_multiple (fun q ls -> Enhanced_vars.add_multiple q ls.Lits.vars) Enhanced_vars.empty x

  let of_vars x =
    Enhanced_vars.fold_multiple (fun q vs -> M.add_multiple q (Lits.vars vs)) M.empty x

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
  module L = Lits_multimap(S)
  module M = Multimap.Make1(T.State_to)(L)

  type t = M.t
  [@@deriving eq, ord]
  type key = Enhanced_lits.t
  type values = S.t

  let pp pp_s ppf x =
    Fmt.pf ppf "%a" (T.State_to.pp (L.pp pp_s)) x

  let empty = M.empty

  let add_multiple k vs x =
    T.State_to.fold (fun q ls -> M.add_multiple q (L.singleton_multiple ls vs)) x k

  let singleton_multiple k vs = add_multiple k vs empty

  let union = M.union
  let (<|>) = M.union

  let find_multiple k x =
    T.State_to.fold (fun q ls ->
        S.union (L.find_multiple ls (M.find_multiple q x)))
      S.empty k

  let find_multiple_or_empty k t =
      try find_multiple k t
      with Not_found -> S.empty

  let strip x =
    T.State_to.fold (Fun.const L.union) L.empty x

  let add_seq_multiple s t =
    Seq.fold_left (fun t (k, v) -> add_multiple k v t) t s

  let of_seq_multiple s =
    add_seq_multiple s empty
end

let refine xs =
  let module R = Refine.Set(Lits) in
  let f = R.refine xs in
  Seq.cons (Lits.comp (R.considered f)) (R.partitions f)

module Production = struct
  type t = T.Labeled_var.t * Rhs.t
  let lhs = fst
  let rhs = snd

  let make lhs rhs = (lhs, rhs)

  let pp ppf (lhs, rhs) =
    Fmt.pf ppf "@[%a ::= %a@]" T.Labeled_var.pp lhs Rhs.pp rhs
end

module M = struct
  include Fa.Make

  let is_final_multiple qs m =
    not (T.States.disjoint qs m.final)

  let adjacent_multiple qs m =
    Seq.flat_map (Fun.flip adjacent m) @@ T.States.to_seq qs

  let labels_multiple qs m =
    T.States.fold (Labels.union % (Fun.flip G.vertex_label m.graph)) Labels.empty qs
end

module M' = Fa.Make0(T.State_pair)(T.State_pairs)(T.State_pair_graph)

module M'' = struct
  include Fa.Make0(T.States)(T.Statess)(T.States_graph)

  let is_final_multiple qs m =
    not (T.Statess.disjoint qs m.final)

  let adjacent_multiple qs m =
    Seq.flat_map (Fun.flip adjacent m) @@ T.Statess.to_seq qs
end

module Subset = M.Gen(T.State_index(Rhs_to))

let is_nullable ps =
  let ns' = T.Vars.of_seq @@ Seq.filter_map (fun p ->
      if R.is_nullable (Production.rhs p)
      then Some (T.Labeled_var.var @@ Production.lhs p)
      else None)
      ps
  in
  T.Vars.of_seq @@ Seq.filter_map (fun p ->
      if R.is_nullable' (Lits.is_nullable (fun v -> T.Vars.mem v ns')) (Production.rhs p)
      then Some (T.Labeled_var.var @@ Production.lhs p)
      else None)
    ps

module Var_to_lits = Multimap.Make1(T.Var_to)(Lits)

let update fs lss =
  Seq.fold_left (fun acc ls ->
      T.Vars.fold (fun v -> Lits.union (Var_to_lits.find_multiple_or ~default:Lits.empty v fs)) acc (Lits.to_vars ls))
    Lits.empty lss

let first n ps =
  let init = Seq.fold_left (fun fs p ->
      Var_to_lits.add_multiple (T.Labeled_var.var @@ Production.lhs p) (Seq.fold_left Lits.union Lits.empty @@ Rhs.first' n @@ Production.rhs p) fs)
      Var_to_lits.empty ps
  in
  Fixedpoint.run ~eq:Var_to_lits.equal (fun fs ->
      Seq.fold_left (fun fs p ->
          Var_to_lits.add_multiple (T.Labeled_var.var @@ Production.lhs p) (update fs (Rhs.first' n @@ Production.rhs p)) fs)
        fs ps)
    init

let productions ps =
  let p = T.Var_to.of_seq @@ Seq.map (fun p -> (T.Labeled_var.var @@ Production.lhs p, Production.rhs p)) ps in
  fun vs ->
    T.Vars.fold (fun v -> R.union (T.Var_to.find v p)) R.nothing vs

let label rhs is_start is_final is_dead lhs q =
  Items.singleton lhs Item_rhs.{state = q; tail = rhs; kernel = not is_start; dead = is_dead; reduce = is_final}

module Dist_rhs = struct
  type t = int * Rhs.t
  [@@deriving ord]
end
module Dist_rhs_to = Balanced_binary_tree.Map.Size(Dist_rhs)

module Lead_rhs = struct
  type t = bool * Rhs.t
  [@@deriving ord]
end
module Lead_rhs_to = Balanced_binary_tree.Map.Size(Lead_rhs)

let convert ~supply (lhs, rhs') =
  let module Gen = M.Gen(T.State_index(Lead_rhs_to)) in
  (lhs, Gen.unfold ~supply ~merge:Lits.union (fun q (_, rhs) ->
       let is_start = rhs == rhs' 
       and is_final = R.is_nullable rhs
       and is_dead = R.is_nothing rhs in
       let next = Seq.filter_map (fun ls ->
           let d = Rhs.simplify @@ Rhs.derivative ls rhs in
           if R.is_nothing d
           then None
           else Some (ls, (false, d)))
           (refine @@ Rhs.first rhs)
       in
       (is_final, label rhs is_start is_final is_dead lhs q, next))
      (true, rhs'))

let convert_multiple ~supply ps =
  Seq.zip (Supply.split supply) ps
  |> Seq.map (fun (supply, p) -> convert ~supply p)

let all_equal_to ~eq xs =
  match Seq.uncons xs with
  | Some (hd, tl) ->
    if Seq.for_all (eq hd) tl
    then `Coherent hd
    else `Incoherent
  | None -> `Source

let distance (lhs, rhs) =
  (lhs, M.fold ~eq:Size.equal (fun _ _ -> Size.top) (fun _ _ adj ->
       match all_equal_to ~eq:Size.equal @@ Seq.map snd adj with
       | `Coherent dist -> Size.succ dist
       | `Incoherent -> Size.top
       | `Source -> Size.zero)
    @@ M.rev rhs)

let distance_multiple ps =
  Seq.map distance ps
  |> T.Labeled_var_to.of_seq

module Acc = Multimap.Make2(T.Var_to)(T.States)

module Enhanced_var = struct
  type t = T.State.t * T.Var.t
  [@@deriving ord]
end

module Enhanced_var_to = Balanced_binary_tree.Map.Size(Enhanced_var)
module Acc2 = Multimap.Make2(Enhanced_var_to)(T.States)

module Enhanced_labeled_var = struct
  type vars = Enhanced_vars.t
  type t = T.State.t * T.Labeled_var.t
  [@@deriving ord]

  let to_vars ((q, v): t) = Enhanced_vars.singleton q (T.Labeled_var.var v)
  let strip ((_, v): t) = v
end

let construct s' ps =
  let starts, finals = Seq.fold_left (fun (starts, finals) (lhs, rhs) ->
      let s = T.Labeled_var.var lhs in
      Acc.add s (M.start rhs) starts, Acc.add_multiple s (M.final rhs) finals)
      (Acc.empty, Acc.empty) ps
  in
  let t = Seq.fold_left (Fun.flip @@ M.sum % Production.rhs) M.empty ps in
  M.{
    start = M.Start.Single (T.States.the (Acc.find_multiple s' starts));
    final = Acc.find_multiple s' finals;
    graph = M.skeleton (Seq.fold_left (fun t (from, _to_, ls) ->
        T.Vars.fold (fun x t ->
            try
            t
            |> M.link ~merge:Lits.union (T.States.singleton from) (Acc.find_multiple x starts) (Lits.call @@ T.Vars.singleton x)
            with Not_found -> t
            (*|> M.link ~merge:Lits.union (Acc.find_multiple x finals) (T.States.singleton to_) (Lits.return (T.Vars.singleton x))*))
          t ls.Lits.vars)
        t (M.transitions t))
  }

let extend m ~tokens ds =
  let starts = Seq.fold_left (fun starts (lhs, rhs) ->
      let s = T.Labeled_var.var lhs in
      Acc.add s (M.start rhs) starts)
      Acc.empty ds
  in
  let t = Seq.fold_left (Fun.flip @@ M.sum % Production.rhs) M.{m with start = M.Start.Multiple (M.start_multiple m)} ds in
  M.{
    m with
    graph = M.skeleton @@
      Seq.fold_left (fun t (from , _, ls) ->
          T.Vars.fold (fun x t ->
              t
              |> M.link ~merge:Lits.union (T.States.singleton from) (Acc.find_multiple x starts) (Lits.call @@ T.Vars.singleton x))
            t (T.Vars.inter tokens ls.Lits.vars))
        t (M.transitions t)
  }

let construct_all tokens s' ps =
  let starts, finals = Seq.fold_left (fun (starts, finals) (lhs, rhs) ->
      let s = T.Labeled_var.var lhs in
      Acc.add s (M.start rhs) starts, Acc.add_multiple s (M.final rhs) finals)
      (Acc.empty, Acc.empty) ps
  in
  let t = Seq.fold_left (Fun.flip @@ M.sum % Production.rhs) M.empty ps in
  M.{
    start = M.Start.Single (T.States.the (Acc.find_multiple s' starts));
    final = Acc.find_multiple s' finals;
    graph = M.skeleton (Seq.fold_left (fun t (from, _to_, ls) ->
        let t = if not (T.Vars.disjoint tokens ls.Lits.vars)
        then
          T.Vars.fold (fun x t ->
              (try
                 t
                 |> M.link ~merge:Lits.union (T.States.singleton from) (Acc.find_multiple x starts) (Lits.call @@ T.Vars.singleton x)
               with Not_found -> t)
              (*|> M.link ~merge:Lits.union (Acc.find_multiple x finals) (T.States.singleton to_) (Lits.return (T.Vars.singleton x))*)
            )
            t tokens
        else t
       in
        T.Vars.fold (fun x t ->
            try
            t
            |> M.link ~merge:Lits.union (T.States.singleton from) (Acc.find_multiple x starts) (Lits.call @@ T.Vars.singleton x)
            with Not_found -> t
            (*|> M.link ~merge:Lits.union (Acc.find_multiple x finals) (T.States.singleton to_) (Lits.return (T.Vars.singleton x))*))
          t (T.Vars.diff ls.Lits.vars tokens)
      )
        t (M.transitions t))
  }

module State_pairs_to = Balanced_binary_tree.Map.Size(T.State_pair)

module Labeled_var_acc = Multimap.Make2(T.Labeled_var_to)(T.State_pairs)

type to_start = Labeled_var_acc.t

let to_start t =
  M.transitions t
  |> Seq.flat_map (fun (q, p, lits) ->
      if Lits.is_scan lits then
        (let items = M.labels p t in
        Seq.filter_map (fun (lhs, rhs) ->
            if rhs.Item_rhs.kernel then None else Some (lhs, (q, p)))
        @@ Items.to_seq items)
      else
        (let items = M.labels q t in
        Seq.filter_map (fun (lhs, rhs) ->
            if rhs.Item_rhs.kernel then None else Some (lhs, (q, q)))
        @@ Items.to_seq items))
  |> Labeled_var_acc.of_seq

let extract state t1 t2  =
  M'.unfold (fun _ (from1, from2) ->
      let next = Seq.product (M.adjacent from1 t1) (M.adjacent from2 t2)
                 |> Seq.filter_map (fun ((to1, c1), (to2, c2)) ->
                     let c = Lits.inter c1 c2 in
                     if Lits.is_empty c (*|| T.Labeled_vars.disjoint (M.labels to1 t1).Labels.predictions (M.labels to2 t2).Labels.predictions*)
                     then None
                     else Some ((Enhanced_lits.singleton from1 c), (to1, to2), (to1, to2)))
      in
      (M.is_final from2 t2, M.labels from2 t2, next))
    (state, M.start t2) (state, M.start t2)

let extract_multiple ps to_start t =
  ps
  |> Seq.flat_map (fun (lhs, rhs) ->
      T.State_pairs.to_seq (Labeled_var_acc.find_multiple lhs to_start)
      |> Seq.map (fun (q, p) -> (((q, lhs): Enhanced_labeled_var.t), extract p t rhs)))

let lookback ps =
  let m = Seq.fold_left (fun acc (((s', lv): Enhanced_labeled_var.t), rhs) ->
      T.Labeled_var_to.add lv ((s', rhs) :: try T.Labeled_var_to.find lv acc with Not_found -> []) acc)
      T.Labeled_var_to.empty ps
  in
  fun lv s q ->
    List.to_seq @@
    List.filter_map (fun (s', rhs) ->
        if M'.state_mem (s, q) rhs
        then Some (s', lv)
        else None)
      (T.Labeled_var_to.find lv m)

module type LITS = sig
  type vars
  type t
  include Re.LITS with type t := t and type vars := vars

  val union: t -> t -> t
  val empty: t
end

module type LHS = sig
  type vars
  type t
  val compare: t -> t -> int
  val to_vars: t -> vars
end

module Analysis(L: LITS)(Lhs: LHS with type vars = L.vars)(A: Fa.S0)(Multimap: functor(S: SET) -> MULTIMAP with type key = L.t with type values = S.t) = struct
  module Lhs_to = Balanced_binary_tree.Map.Size(Lhs)

  module type S = sig
    module M: sig
      type t
      type key
      type values
    end

    val per_state: (Lhs.t * (A.Start.single, _, M.key) A.t) Seq.t -> M.t -> (Lhs.t * (A.state -> M.values)) Seq.t
    val per_lits: (Lhs.t * (A.Start.single, _, M.key) A.t) Seq.t -> M.t -> M.t
  end

  module Abstract(S: SET) = struct
    module M = Multimap(S)

    module type ALG = sig
      val start: _ A.t -> A.state -> _ -> M.values
      val next: M.t -> M.values -> _ -> (L.t * (unit -> M.values)) Seq.t -> M.values
    end

    module type S = S with module M := M

    let make (module Alg: ALG) =
      (module struct
        let compute_single_source _ rhs self =
          A.dijkstra (Alg.start rhs) (Alg.next self) rhs

        let compute_multi_source _ rhs self =
          A.digraph (Alg.start rhs) (Alg.next self) rhs

        let per_state ps self =
          Seq.map (fun (lhs, rhs) -> (lhs, compute_multi_source lhs rhs self)) ps

        let update ps self =
          Seq.fold_left (fun acc (lhs, rhs) ->
              M.add_multiple (L.of_vars @@ Lhs.to_vars @@ lhs) (compute_single_source lhs rhs self (A.start @@ rhs)) acc)
            self ps

        let per_lits ps self =
          Fixedpoint.run ~eq:M.equal (update ps) self
      end: S)
  end

  module Nullable = struct
    include Abstract(struct
        type t = bool
        [@@deriving eq, ord]

        let union = (||)
        let empty = false
      end)

    let get = make (module struct
        let start a q _ =
          A.is_final q a

        let next self current _ adj =
          Seq.fold_left (fun acc (x, visit) -> M.find_multiple_or_empty x self && visit () || acc)
            current adj
      end)

    let find x nullable =
      M.find_multiple_or_empty x nullable
  end

  module First = struct
    include Abstract(L)

    let get nullable = make (module struct
        let start _ _ _ =
          L.empty

        let next self current _ adj =
          Seq.fold_left (fun acc (x, visit) ->
              let (<|>) = L.union in
              M.find_multiple_or_empty x self <|> acc <|> if nullable x then visit () else L.empty)
            current adj
      end)
  end

  module Follow = struct
    module M = Multimap(L)

    module type S = S with module M := M

    let get nullable first = (module struct
      let compute lhs _ self q =
        L.union (first lhs q) (if nullable lhs q then M.find_multiple_or_empty (L.of_vars @@ Lhs.to_vars @@ lhs) self else L.empty)

      let per_state ps self =
        Seq.map (fun (lhs, rhs) -> (lhs, compute lhs rhs self)) ps

      let update ps self =
        Seq.fold_left (fun acc (lhs, rhs) ->
            Seq.fold_left (fun acc (_, q, x) ->
                M.add_multiple x (compute lhs rhs self q) acc)
              acc (A.transitions rhs))
          self ps

      let per_lits ps self =
        Fixedpoint.run ~eq:M.equal (update ps) self
    end: S)
  end

  type t =
    {
      nullable: (module Nullable.S) Lazy.t;
      nullable_per_lits: Nullable.M.t Lazy.t;
      nullable_per_state: (A.state -> bool) Lhs_to.t Lazy.t;
      first: (module First.S) Lazy.t;
      first_per_lits: First.M.t Lazy.t;
      first_per_state: (A.state -> L.t) Lhs_to.t Lazy.t;
      follow: (module Follow.S) Lazy.t;
      follow_per_lits: Follow.M.t Lazy.t;
      follow_per_state: (A.state -> L.t) Lhs_to.t Lazy.t
    }

  let analyse ?(nullable_start = Nullable.M.empty) ?(first_start = First.M.empty) ?(follow_start = Follow.M.empty) ps =
    let rec this = 
      {
        nullable = lazy Nullable.get;
        nullable_per_lits = lazy begin
          let (lazy (module X)) = this.nullable in 
          X.per_lits ps nullable_start
        end;
        nullable_per_state = lazy begin
          let (lazy (module X)) = this.nullable
          and (lazy nullable) = this.nullable_per_lits in
          Lhs_to.of_seq @@ X.per_state ps nullable
        end;
        first = lazy begin
          let (lazy nullable) = this.nullable_per_lits in 
          First.get (fun ls -> Nullable.find ls nullable)
        end;
        first_per_lits = lazy begin
          let (lazy (module X)) = this.first in
          X.per_lits ps first_start
        end;
        first_per_state = lazy begin
          let (lazy (module X)) = this.first in
          let (lazy first) = this.first_per_lits in
          Lhs_to.of_seq @@ X.per_state ps first
        end;
        follow = lazy begin
          let (lazy nullable) = this.nullable_per_state in
          let (lazy first) = this.first_per_state in
          Follow.get (fun lhs q -> Lhs_to.find lhs nullable q) (fun lhs q -> Lhs_to.find lhs first q)
        end;
        follow_per_lits = lazy begin
          let (lazy (module X)) = this.follow in
          X.per_lits ps follow_start
        end;
        follow_per_state = lazy begin
          let (lazy (module X)) = this.follow in
          let (lazy follow) = this.follow_per_lits in
          Lhs_to.of_seq @@ X.per_state ps follow
        end
      }
    in this

  let nullable_per_lits lhs r =
    let (lazy nullable) = r.nullable_per_lits in
    Nullable.M.find_multiple_or_empty lhs nullable

  let nullable_per_state lhs q r =
    let (lazy nullable) = r.nullable_per_state in
    Lhs_to.find lhs nullable q

  let follow_per_lits lhs r =
    let (lazy follow) = r.follow_per_lits in
    Follow.M.find_multiple_or_empty lhs follow

  let follow_per_state lhs q r =
    let (lazy follow) = r.follow_per_state in
    Lhs_to.find lhs follow q
end

module Enhanced_analysis = Analysis(Enhanced_lits)(Enhanced_labeled_var)(M')(Enhanced_lits_multimap)

type lookahead =
  {
    right_nulled: bool;
    shift_lookahead: Enhanced_lits.t;
    reduce_lookahead: Enhanced_lits.t;
  }

let lookahead' lb r lv s q =
  let lhss = lb lv s q in
  {
    right_nulled =
      Seq.fold_left (||) false @@
      Seq.map (fun lhs ->
          Enhanced_analysis.nullable_per_state lhs (s, q) r)
        lhss;
    shift_lookahead =
      Seq.fold_left Enhanced_lits.union Enhanced_lits.empty @@
      Seq.map (fun lhs ->
          Enhanced_analysis.follow_per_state lhs (s, q) r)
        lhss;
    reduce_lookahead =
      Seq.fold_left Enhanced_lits.union Enhanced_lits.empty @@
      Seq.filter_map (fun lhs ->
          if Enhanced_analysis.nullable_per_state lhs (s, q) r
          then Some (Enhanced_analysis.follow_per_lits (Enhanced_lits.of_vars @@ Enhanced_labeled_var.to_vars lhs) r)
          else None)
        lhss;
  }

let nullable r q ls =
  Enhanced_analysis.nullable_per_lits (Enhanced_lits.singleton q ls) r

module Symbol_analysis = struct
  type t = {symbols: Enhanced_lits.t; nonterminal: Enhanced_lits.t} (* terminal: Enhanced_lits.t}*)

  let lhs_symbols ps =
    ps
    |> Seq.map (fun (lhs, _) -> Enhanced_lits.of_vars @@ Enhanced_labeled_var.to_vars lhs)
    |> Seq.fold_left Enhanced_lits.union Enhanced_lits.empty

  let rhs_symbols ps =
    ps
    |> Seq.map (fun (_, rhs) -> Seq.fold_left Enhanced_lits.union Enhanced_lits.empty @@ rhs)
    |> Seq.fold_left Enhanced_lits.union Enhanced_lits.empty

  let make ps =
    let l = lhs_symbols ps in
    let r = rhs_symbols ps in
    {symbols = Enhanced_lits.union l r; nonterminal = l}
end

let run_analysis ps =
  let module X = Enhanced_analysis.First.M in
  (*let sa = Symbol_analysis.make (Seq.map (fun (lhs, rhs) -> (lhs, M'.all_lits rhs)) ps) in*)
  let vs = Seq.fold_left (fun acc (lhs, _) ->
      let x = Enhanced_lits.of_vars @@ Enhanced_labeled_var.to_vars lhs in
      X.add_multiple x x acc) X.empty ps in

  let cs =
    ps
    |> Seq.flat_map (fun (_, (rhs: _ M'.t)) ->
        Seq.map (fun (s, lits) -> Enhanced_lits.singleton s (Lits.codes @@ lits.Lits.codes))
          (Seq.flat_map Enhanced_lits.to_seq (M'.all_lits rhs)))
    |> Seq.fold_left (fun acc x ->
        X.add_multiple x x acc) X.empty
  in
  let first_start = X.union vs cs in
  Enhanced_analysis.analyse ~first_start ps

let lookahead_tokens tokens la lv s q =
  let x = la lv s q in
  Fmt.pr ">>>>>> %d > %d (%a) (%a)@." s q Enhanced_lits.pp x.shift_lookahead Enhanced_lits.pp x.reduce_lookahead;
  Enhanced_lits.union x.shift_lookahead x.reduce_lookahead
  |> Enhanced_lits.fold (fun s ls ->
      let cs = Lits.codes (ls.Lits.codes) in
      let vs = Lits.inter ls (Lits.of_vars tokens) in
      Enhanced_lits.union (Enhanced_lits.singleton s (Lits.union vs cs)))
    Enhanced_lits.empty
  (*|> Enhanced_lits.filter (fun _ ls -> not @@ Lits.is_empty (Lits.inter ls (Lits.of_vars tokens)))*)

type enhanced_vars = Enhanced_vars.t
type enhanced_labeled_var = Enhanced_labeled_var.t

module Enhanced_labeld_var_to = Balanced_binary_tree.Map.Size(Enhanced_labeled_var)

let strip ~supply t =
  let module Gen = M.Gen(T.State_index(T.State_pair_to)) in
  Gen.unfold ~supply ~merge:Lits.union (fun _ from ->
      let next =
        M'.adjacent from t
        |> Seq.map (fun (s, ls') -> (Enhanced_lits.strip ls', s))
      in
      (M'.is_final from t, M'.labels from t, next))
    (M'.start t)

module PG = T.State_pair_graph

let inter m1 m2 =
  let start = (M.start m1, M.start m2) in
  (start,
   PG.unfold (fun _ (from1, from2) ->
       let adj = Seq.product (M.adjacent from1 m1) (M.adjacent from2 m2)
                 |> Seq.filter_map (fun ((to1, c1), (to2, c2)) ->
                     let c = Lits.inter c1 c2 in
                     if Lits.is_empty c (*|| T.Labeled_vars.disjoint (M.labels to1 m1).Labels.predictions (M.labels to2 m2).Labels.predictions*)
                     then None
                     else Some (c, (to1, to2), (to1, to2)))
       in
       let adj' = 
         M.adjacent from2 m2
         |> Seq.filter_map (fun (s, ls) -> if Lits.is_call ls then Some (ls, (from1, s), (from1, s)) else None)
       in
       let (_, rhs) = Items.the (M.labels from2 m2) in
       (rhs.Item_rhs.tail, Seq.(adj @ adj')))
     start start)

let backlog g =
  PG.labeled_edges_map (fun s _ ls ->
      match T.Vars.choose ls.Lits.call with
      | Some v ->
        Seq.fold_left R.union R.nothing @@
        Seq.filter_map (fun (q, ls') ->
            if Lits.subset ls' (Lits.var v)
            then Some (PG.vertex_label q g)
            else None)
          (PG.adjacent s g)
      | None -> R.null)
    g

module Kleene = Re.Kleene(T.Vars)(Lits)(T.State_pair_graph)

let rev_solve = Kleene.rev_solve 

(*let right_context (start, g) =
  fun s q -> R.concat (PG.vertex_label (s, q) g) (PG.edge_label start (s, q) g)*)

let right_context (start, g) =
  fun s q -> PG.edge_label start (s, q) g

module States_to = Balanced_binary_tree.Map.Size(T.States)

let erase_call t =
  M.homomorphism (fun ls -> if Lits.is_call ls then Lits.null else ls) t

let erase_return t =
  M.homomorphism (fun ls -> if Lits.is_return ls then Lits.null else ls) t

let closure t qs =
  let module C = Closure.Make(T.States) in
  C.closure (fun q -> M.goto q Lits.is_call t) qs

let closure' t qs =
  let module C = Closure.Make(T.States) in
  C.closure (fun q -> M.goto q Lits.is_null t) qs

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
  M.states_labels t
  |> Seq.map (fun (s, items) -> (Items.matches items, T.States.singleton s))
  |> R'.refine
  |> R'.partitions

let partition t =
  let module H = Refine.Hopcroft(T.States) in

  let initial =
    Seq.cons (T.States.of_seq @@ M.states t) (Seq.map snd @@ group_states_by_output t)
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
        |> Seq.filter_map (fun (s, ls) -> if Lits.is_call ls then None else Some (ls, T.States.singleton s))
        |> R.refine
        |> R.partitions
      in
      (M.is_final_multiple from t, M.labels_multiple from t, next))
    (closure t @@ M.start_multiple t)

module Items_nc = Multimap.Make1(T.State_to)(Items)

(*let noncannonical ~supply token_lookahead t =
  let module Gen = M.Gen(T.State_index(States_to)) in
  Gen.unfold ~supply ~merge:Lits.union (fun _ from ->
      let module R = Refine.Map(Lits)(T.States) in
      let nc =
        T.States.to_seq from
        |> Seq.flat_map (fun s ->
            (M.labels s t)
            |> Items.to_seq
            |> Seq.map (fun (lhs, rhs) -> token_lookahead lhs s (rhs.Item_rhs.state)))
        |> Seq.fold_left Enhanced_lits.union Enhanced_lits.empty
      in
      let nc' = Enhanced_lits.to_seq nc
                |> Seq.flat_map (fun (s, ls) ->
                    let ls = Lits.vars ls.Lits.vars in
                    (Seq.flat_map (fun (_, ls') ->
                         if Lits.subset ls' ls then
                           (*Seq.append*) (* XXX: for infinite lookahead version *)
                             (Seq.map (fun (q, ls'') ->
                                   (q, Lits.scan (ls''.Lits.scan)))
                                 (M.adjacent s t))
                             (*(Seq.return (p, ls'))*)
                         else Seq.empty)
                        (M.adjacent s t)))
      in

      Fmt.pr "(((((%a)))))" (Fmt.seq (Fmt.parens (Fmt.pair T.State.pp Lits.pp))) nc';

      let next =
        Seq.append (M.adjacent_multiple from t) nc'
        |> Seq.map (fun (s, ls) -> (ls, T.States.singleton s))
        |> R.refine
        |> R.partitions
      in
      let labels = T.States.fold (fun s -> Items_nc.add_multiple s (M.labels s t)) Items_nc.empty from in
      (M.is_final_multiple from t, labels, next))
    (M.start_multiple t)*)

let noncannonical token_lookahead t =
  M''.unfold (fun _ from ->
      let module R = Refine.Map(Lits)(T.States) in
      let nc =
        T.States.to_seq from
        |> Seq.flat_map (fun s ->
            (M.labels s t)
            |> Items.to_seq
            |> Seq.map (fun (lhs, rhs) -> token_lookahead lhs s (rhs.Item_rhs.state)))
        |> Seq.fold_left Enhanced_lits.union Enhanced_lits.empty
      in
      let nc' = Enhanced_lits.to_seq nc
                |> Seq.flat_map (fun (s, ls) ->
                    let ls = Lits.vars ls.Lits.vars in
                    (Seq.flat_map (fun (_, ls') ->
                         if Lits.subset ls' ls then
                           (*Seq.append*) (* XXX: for infinite lookahead version *)
                             (Seq.map (fun (q, ls'') ->
                                   (q, Lits.scan (ls''.Lits.scan)))
                                 (M.adjacent s t))
                             (*(Seq.return (p, ls'))*)
                         else Seq.empty)
                        (M.adjacent s t)))
      in

      Fmt.pr "(((((%a)))))" (Fmt.seq (Fmt.parens (Fmt.pair T.State.pp Lits.pp))) nc';

      let next =
        Seq.append (M.adjacent_multiple from t) nc'
        |> Seq.map (fun (s, ls) -> (ls, T.States.singleton s))
        |> R.refine
        |> R.partitions
        |> Seq.map (fun (ls, ss) -> (ls, ss, ss))
      in
      let labels = T.States.fold (fun s -> Items_nc.add_multiple s (M.labels s t)) Items_nc.empty from in
      (M.is_final_multiple from t, labels, next))
    (M.start_multiple t) (M.start_multiple t)

let noncannonical2 token_lookahead t =
  M.{
    t with
    graph =
      (Seq.fold_left (fun g from ->
           let la =
             (M.labels from t)
             |> Items.to_seq
             |> Seq.map (fun (lhs, rhs) -> token_lookahead lhs from (rhs.Item_rhs.state))
             |> Seq.fold_left Enhanced_lits.union Enhanced_lits.empty
           in
           let nc' = Enhanced_lits.to_seq la
                     |> Seq.flat_map (fun (s, ls) ->
                         let ls = Lits.vars ls.Lits.vars in
                         (Seq.flat_map (fun (_, ls') ->
                              if Lits.subset ls' ls then
                                (*Seq.append*) (* XXX: for infinite lookahead version *)
                                (Seq.map (fun (q, ls'') ->
                                     (q, Lits.scan (ls''.Lits.scan)))
                                    (M.adjacent s t))
                                (*(Seq.return (p, ls'))*)
                              else Seq.empty)
                             (M.adjacent s t)))
           in
           Seq.fold_left (fun g (s, ls) ->
               if not (Lits.is_empty ls) then
                 T.State_graph.connect from s ls g
               else g)
             g nc')
          t.M.graph (M.states t))
  }

let upgrade t =
  M''.unfold (fun _ from ->
      let next =
        M.adjacent_multiple from t
        |> Seq.map (fun (s, ls) ->
            (ls, T.States.singleton s, T.States.singleton s))
      in
      let labels = T.States.fold (fun s -> Items_nc.add_multiple s (M.labels s t)) Items_nc.empty from in
      (M.is_final_multiple from t, labels, next))
    (T.States.singleton (M.start t)) (T.States.singleton (M.start t))

let back ~is_token eps =
  M'.rev @@
  Seq.fold_left (fun g (lhs, rhs) ->
      if not @@ is_token @@ T.Labeled_var.var @@ Enhanced_labeled_var.strip lhs then
        M'.merge ~merge_labels:Labels.union ~merge_lits:Enhanced_lits.union rhs g
      else g)
    M'.empty eps

(*
let noncannonical ~supply token_lookahead t =
  let module Gen = Noncannonical.Gen(T.State_index(States_to)) in
  Gen.unfold ~supply ~merge:Lits.union (fun _ from ->
      let module R = Refine.Map(Lits)(T.States) in
      let (nc: T.States.t) =
        T.States.to_seq from
        |> Seq.flat_map (fun s ->
            (M.labels s t).Labels.items
            |> Items.to_seq
            |> Seq.map (fun (lv, q) -> token_lookahead lv s q))
        |> Seq.fold_left Enhanced_lits.union Enhanced_lits.empty
        |> Enhanced_lits.to_seq
        |> Seq.map (fun (s, _) -> s)
        |> T.States.of_seq
      in
      let (next: (Lits.t * T.States.t) Seq.t) =
        M.adjacent_multiple from t
        |> Seq.map (fun (s, ls) -> (ls, T.States.singleton s))
        |> R.refine
        |> R.partitions
      in
      let next = Seq.cons (Lits.null, nc) next in
      (M.is_final_multiple from t, from, next))
    (M.start_multiple t)*)

(*let lookahead rc g =
  {
    start = M.start g;
    final = M.final g;
    graph = G.labeled_vertices_map (fun s ls ->
        ls.Labels.items
        |> Items.to_seq
        |> Seq.filter_map (fun (lhs, q) ->
            if T.Labeled_vars.mem lhs ls.Labels.matches
            then Some (lhs, Rhs.simplify @@ rc s q)
            else None)
        |> T.Labeled_var_to.of_seq) g.graph
  }*)

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
          |> Seq.filter_map (fun (s, ls) -> if Lits.is_call ls then None else Some (ls, T.States.singleton s))
          |> R.refine
          |> R.partitions
          |> Seq.map (fun (ls, to_) -> (ls, (false, to_)))
        else begin
          let calls, others =
            M.adjacent_multiple from t
            |> Seq.map (fun (to_, ls) -> (ls, T.States.singleton to_))
            |> R.refine
            |> R.partitions
            |> Seq.partition (fun (ls, _) -> Lits.is_call ls)
          in
          Seq.(Seq.map (fun (ls, to_) -> (ls, (false, to_))) others @ Seq.map (fun (ls, to_) -> (ls, (true, closure t to_))) calls)
        end
      in
      (M.is_final_multiple from t, M.labels_multiple from t, next))
    (false, M.start_multiple t)

let labels_multiple' qs m =
  T.Statess.fold (Items_nc.union % (Fun.flip M''.labels m)) Items_nc.empty qs

let collapse' t =
  let module R = Refine.Map(Lits)(T.Statess) in

  M''.unfold  (fun _ from ->
      let next =
        M''.adjacent_multiple from t
        |> Seq.map (fun (s, ls) -> (ls, T.Statess.singleton s))
        |> R.refine
        |> R.partitions
        |> Seq.map (fun (ls, ss) -> (ls, T.Statess.fold T.States.union T.States.empty ss, ss))
      in
      (M''.is_final_multiple from t, labels_multiple' from t, next))
      (M''.start t) (T.Statess.singleton (M''.start t)) 

module Builder = struct
  module Item_rhs' = struct
    type t = {base: Item_rhs.t; right_nulled: bool; shift_lookahead: Lits.t; reduce_lookahead: Lits.t; distance: Size.t; next: Lits.t; next_null: (T.Labeled_var.t * Lits.t) list; state_pair: T.State_pair.t; reminder: T.Var.t list list}
    [@@deriving eq, ord]
  end

  module Items' = struct
    type t = (T.Labeled_var.t * Item_rhs'.t) list
    [@@deriving eq, ord]

    let union x y = List.append x y
    let empty = []
    let to_seq = List.to_seq

    type item = T.Labeled_var.t * Item_rhs'.t
    let output (lhs, _) = lhs
    let is_dead (_, rhs) = rhs.Item_rhs'.base.dead
    let is_kernel (_, rhs) = rhs.Item_rhs'.base.kernel
    let is_reduce (_, rhs) = rhs.Item_rhs'.base.reduce
    let is_right_nulled (_, rhs) = rhs.Item_rhs'.right_nulled
    let shift (_, rhs) = rhs.Item_rhs'.next
    let null (_, rhs) = List.to_seq @@ rhs.Item_rhs'.next_null
    let shift_lookahead (_, rhs) = rhs.Item_rhs'.shift_lookahead
    let reduce_lookahead (_, rhs) = rhs.Item_rhs'.reduce_lookahead
    let distance (_, rhs) = rhs.Item_rhs'.distance
    let state_pair (_, rhs) = rhs.Item_rhs'.state_pair
    let reminder (_, rhs) = rhs.Item_rhs'.reminder
  end

  let make ~tokens start ps ds =
    (*let supply1, supply2 = Supply.split2 T.State.supply in
    let ps' = convert_multiple ~supply:supply1 ps in
    let ds' = convert_multiple ~supply:supply2 ds in
    let ps_to = T.Labeled_var_to.of_seq Seq.(ps' @ ds') in
    let m = construct_all tokens start Seq.(ps' @ ds') in
    (*let m = extend m ~tokens ds' in *)
    let m = M.homomorphism (fun lits -> if Lits.is_call lits && T.Vars.subset lits.call tokens then Lits.scan (lits.call) else lits) m in

    Fmt.pr "%s@." @@
    Dot.string_of_graph @@
    M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Lits.pp x) m;

    let m' = subset ~supply:T.State.supply m in

    Fmt.pr "%s@." @@
    Dot.string_of_graph @@
    M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Lits.pp x) m';

    let to_start = to_start m' in
    let eps = extract_multiple Seq.(ps' @ ds') to_start m' in

    Seq.iter (fun ((q, lv), x) ->
        Fmt.pr "(%a %a) %s@." T.State.pp q T.Labeled_var.pp lv @@
        Dot.string_of_graph @@
        M'.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Enhanced_lits.pp x) x)
      eps;

    Fmt.pr "LA@.";
    let analysis = run_analysis eps in
    let lb = lookback eps in
    let la = lookahead' lb analysis in
    let _nl = nullable analysis in

    Fmt.pr "DIST@.";
    let dist = distance_multiple Seq.(ps' @ ds') in

    Seq.iter (fun (lhs, rhs) ->
        let d = T.Labeled_var_to.find lhs dist in
        Fmt.pr "%s@." @@
        Dot.string_of_graph @@
        M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Size.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Lits.pp x) (M.map_states_labels (fun s _ -> d s) rhs))
      ps';

    Fmt.pr "TOKEN LA@.";
    let token_la = lookahead_tokens tokens la in

    Fmt.pr "NC@.";
    let nc = noncannonical token_la m' in
    Fmt.pr "%s@." @@
    Dot.string_of_graph @@
    M''.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string (T.State_to.pp Items.pp) x) ~string_of_lits:(fun x -> Fmt.to_to_string Lits.pp x) nc;

    Fmt.pr "NC@.";
    let nc = M''.homomorphism (fun lits -> if Lits.is_scan lits then {lits with null = true; scan = T.Vars.empty} else lits) nc in

    let nc = collapse' nc in

    Fmt.pr "%s@." @@
    Dot.string_of_graph @@
    M''.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string (T.State_to.pp Items.pp) x) ~string_of_lits:(fun x -> Fmt.to_to_string Lits.pp x) nc;

    let b = back ~is_token:(fun v -> T.Vars.mem v tokens) eps in
    Fmt.pr "%s@." @@
    Dot.string_of_graph @@
    M'.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Enhanced_lits.pp x) b;*)

    let supply1, supply2 = Supply.split2 T.State.supply in
    let ps' = convert_multiple ~supply:supply1 ps in
    let ds' = convert_multiple ~supply:supply2 ds in
    let ps_to = T.Labeled_var_to.of_seq Seq.(ps' @ ds') in
    let m = construct start Seq.(ps' @ ds') in
    (*let m = extend m ~tokens ds' in *)
    let m = M.homomorphism (fun lits -> if Lits.is_call lits && T.Vars.subset lits.call tokens then Lits.scan (lits.call) else lits) m in

    Fmt.pr "%s@." @@
    Dot.string_of_graph @@
    M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Lits.pp x) m;

    let m' = subset ~supply:T.State.supply m in

    Fmt.pr "%s@." @@
    Dot.string_of_graph @@
    M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Lits.pp x) m';

    let to_start = to_start m' in
    let eps = extract_multiple Seq.(ps' @ ds') to_start m' in

    Seq.iter (fun ((q, lv), x) ->
        Fmt.pr "(%a %a) %s@." T.State.pp q T.Labeled_var.pp lv @@
        Dot.string_of_graph @@
        M'.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Enhanced_lits.pp x) x)
      eps;

    Fmt.pr "LA@.";
    let analysis = run_analysis eps in
    let lb = lookback eps in
    let la = lookahead' lb analysis in
    let _nl = nullable analysis in

    Fmt.pr "DIST@.";
    let dist = distance_multiple Seq.(ps' @ ds') in

    Seq.iter (fun (lhs, rhs) ->
        Fmt.pr "-- %a@." T.Labeled_var.pp lhs;
        let d = T.Labeled_var_to.find lhs dist in
        Fmt.pr "%s@." @@
        Dot.string_of_graph @@
        M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Size.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Lits.pp x) (M.map_states_labels (fun s _ -> d s) rhs))
      ps';

    Fmt.pr "TOKEN LA@.";
    let token_la = lookahead_tokens tokens la in

    Fmt.pr "NC@.";
    let nc = noncannonical2 token_la m' in
    let nc = upgrade nc in

    Fmt.pr "%s@." @@
    Dot.string_of_graph @@
    M''.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string (T.State_to.pp Items.pp) x) ~string_of_lits:(fun x -> Fmt.to_to_string Lits.pp x) nc;

    Fmt.pr "NC@.";
    let nc = M''.homomorphism (fun lits -> if Lits.is_scan lits then {lits with null = true; scan = T.Vars.empty} else lits) nc in

    (*let nc = collapse' nc in*)

    Fmt.pr "%s@." @@
    Dot.string_of_graph @@
    M''.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string (T.State_to.pp Items.pp) x) ~string_of_lits:(fun x -> Fmt.to_to_string Lits.pp x) nc;

    let b = back ~is_token:(fun v -> T.Vars.mem v tokens) eps in
    Fmt.pr "%s@." @@
    Dot.string_of_graph @@
    M'.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Enhanced_lits.pp x) b;

    (*let supply1, supply2 = Supply.split2 T.State.supply in
    let ps' = convert_multiple ~supply:supply1 ps in
    let ds' = convert_multiple ~supply:supply2 ds in
    let ps_to = T.Labeled_var_to.of_seq Seq.(ps' @ ds') in
    let m = construct start ps' in

    Fmt.pr "%s@." @@
    Dot.string_of_graph @@
    M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Lits.pp x) m;

    let m' = subset ~supply:T.State.supply m in

    let m' = extend m' ~tokens ds' in 
    let m' = M.homomorphism (fun lits -> if Lits.is_call lits && T.Vars.subset lits.call tokens then Lits.scan (lits.call) else lits) m' in

    Fmt.pr "%s@." @@
    Dot.string_of_graph @@
    M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Lits.pp x) m';

    let to_start = to_start m' in
    let eps = extract_multiple Seq.(ps' @ ds') to_start m' in

    Seq.iter (fun ((q, lv), x) ->
        Fmt.pr "(%a %a) %s@." T.State.pp q T.Labeled_var.pp lv @@
        Dot.string_of_graph @@
        M'.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Enhanced_lits.pp x) x)
      eps;

    Fmt.pr "LA@.";
    let analysis = run_analysis eps in
    let lb = lookback eps in
    let la = lookahead' lb analysis in
    let _nl = nullable analysis in

    Fmt.pr "DIST@.";
    let dist = distance_multiple Seq.(ps' @ ds') in

    Seq.iter (fun (lhs, rhs) ->
        let d = T.Labeled_var_to.find lhs dist in
        Fmt.pr "%s@." @@
        Dot.string_of_graph @@
        M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Size.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Lits.pp x) (M.map_states_labels (fun s _ -> d s) rhs))
      ps';

    Fmt.pr "TOKEN LA@.";
    let token_la = lookahead_tokens tokens la in

    Fmt.pr "NC'''@.";
    let nc =noncannonical2 token_la m' in

    Fmt.pr "%s@." @@
    Dot.string_of_graph @@
    M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string (Items.pp) x) ~string_of_lits:(fun x -> Fmt.to_to_string Lits.pp x) nc;

    let nc = upgrade nc in
    Fmt.pr "%s@." @@
    Dot.string_of_graph @@
    M''.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string (T.State_to.pp Items.pp) x) ~string_of_lits:(fun x -> Fmt.to_to_string Lits.pp x) nc;

    Fmt.pr "NC@.";
    let nc = M''.homomorphism (fun lits -> if Lits.is_scan lits then {lits with null = true; scan = T.Vars.empty} else lits) nc in

    (*let nc = collapse' nc in*)

    Fmt.pr "%s@." @@
    Dot.string_of_graph @@
    M''.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string (T.State_to.pp Items.pp) x) ~string_of_lits:(fun x -> Fmt.to_to_string Lits.pp x) nc;

    let b = back ~is_token:(fun v -> T.Vars.mem v tokens) eps in
    Fmt.pr "%s@." @@
    Dot.string_of_graph @@
    M'.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Enhanced_lits.pp x) b;*)

    let t = M''.map_states_labels (fun r nc_items ->
        T.State_to.fold (fun s items acc ->
            List.append acc @@
            List.of_seq @@
            (Items.to_seq items |> Seq.map (fun (lhs, rhs) ->
                 (lhs,
                  Item_rhs'.{
                    base = rhs;
                    next = Seq.fold_left Lits.union Lits.empty
                      @@ Seq.map snd
                      @@ M.adjacent rhs.Item_rhs.state 
                      @@ T.Labeled_var_to.find lhs ps_to;
                    next_null = List.of_seq @@
                      Seq.flat_map (fun (s, ls) ->
                          if Lits.is_null ls then
                            let nc_items = M''.labels s nc in
                            T.State_to.to_seq nc_items |> Seq.flat_map (fun (s, items) ->
                                Items.to_seq items |> Seq.filter_map (fun (lhs, rhs) ->
                                    if not rhs.Item_rhs.kernel && rhs.Item_rhs.reduce then
                                      Some (lhs, Enhanced_lits.strip @@ (la lhs s rhs.Item_rhs.state).reduce_lookahead)
                                    else None))
                          else Seq.empty)
                        (M''.adjacent r nc);
                    reminder =
                      if not @@ T.Vars.mem (T.Labeled_var.var lhs) tokens && (la lhs s rhs.Item_rhs.state).right_nulled && not (R.is_nullable rhs.Item_rhs.tail) then begin
                        Fmt.pr "%a" Rhs.pp rhs.Item_rhs.tail;
                        rhs.Item_rhs.tail
                        |> Rhs.to_seq (fun ls -> T.Vars.to_seq ls.Lits.vars)
                        |> Seq.map List.of_seq
                        |> List.of_seq end
                      else
                        [[]];
                    right_nulled = (la lhs s rhs.Item_rhs.state).right_nulled;
                    shift_lookahead = Enhanced_lits.strip @@ (la lhs s rhs.Item_rhs.state).shift_lookahead;
                    reduce_lookahead = Enhanced_lits.strip @@ (la lhs s rhs.Item_rhs.state).reduce_lookahead;
                    distance = (T.Labeled_var_to.find lhs dist) rhs.Item_rhs.state;
                    state_pair = (s, rhs.Item_rhs.state)
                  }))))
          [] nc_items)
        nc
    in
    (b, t)
end

    (*{
      start = M.start nc;
      final = M.final nc;
      graph = G.labeled_vertices_map (fun _ items ->
          Items.to_seq items
          |> Seq.map (fun (lhs, rhs) -> (lhs, rhs))
          |> T.Labeled_var_to.of_seq)
          nc.graph
    }*)




              (*if T.Labeled_vars.mem lhs ls.Labels.matches
              then Some (lhs, Rhs.simplify @@ rc s q)
              else None)*)
