open Te_bot
open! Prelude
module T = Types
module G = T.State_graph

let pp_if b pp ppf  =
  if b then Fmt.pf ppf "%a@ " pp else Fmt.nop ppf

module Lits: sig
  type vars = T.Vars.t
  type t = {eof: bool; call: T.Vars.t; return: T.Vars.t; null: bool; vars: T.Vars.t; codes: T.Codes.t}
  include Re.LITS with type t := t and type vars := vars
  include Refine.PARTITION with type t := t
  val comp: t -> t
  val is_return: t -> bool
  val is_call: t -> bool
  val is_null: t -> bool
  val is_nullable: (T.Var.t -> bool) -> t -> bool

  val eof: t
  val empty: t
  val null: t
  val call: T.Vars.t -> t
  val return: T.Vars.t -> t
  val var: T.Var.t -> t
  val vars: T.Vars.t -> t
  val codes: T.Codes.t -> t
end = struct
  type vars = T.Vars.t
  type t = {eof: bool; call: T.Vars.t; return: T.Vars.t; null: bool; vars: T.Vars.t; codes: T.Codes.t}
  [@@deriving eq, ord]

  let pp ppf x =
    Fmt.pf ppf "@[%a%a%a%a%a%a@]"
      (pp_if x.eof Fmt.string) "$"
      (pp_if (not @@ T.Vars.is_empty x.call) (fun ppf -> Fmt.pf ppf "◂ %a" T.Vars.pp)) x.call
      (pp_if x.null Fmt.string) "ε"
      (pp_if (not @@ T.Vars.is_empty x.vars) T.Vars.pp) x.vars
      (pp_if (not @@ T.Codes.is_empty x.codes) T.Codes.pp) x.codes
      (pp_if (not @@ T.Vars.is_empty x.return) (fun ppf -> Fmt.pf ppf "▸ %a" T.Vars.pp)) x.return

  let subset x y =
    Bool.imp x.eof y.eof &&
    T.Vars.subset x.call y.call &&
    T.Vars.subset x.return y.return &&
    Bool.imp x.null y.null &&
    T.Vars.subset x.vars y.vars &&
    T.Codes.subset x.codes y.codes

  let empty =
    {
      eof = false;
      call = T.Vars.empty;
      return = T.Vars.empty;
      null = false;
      vars = T.Vars.empty;
      codes = T.Codes.empty;
    }

  let is_empty x =
    not x.eof &&
    T.Vars.is_empty x.call &&
    T.Vars.is_empty x.return &&
    not x.null &&
    T.Vars.is_empty x.vars &&
    T.Codes.is_empty x.codes

  let is_return x =
    not (T.Vars.is_empty x.return)

  let is_call x =
    not (T.Vars.is_empty x.call)

  let is_null x =
    x.null

  let eof = {empty with eof = true}
  let call x = {empty with call = x}
  let return x = {empty with return = x}
  let null = {empty with null = true}
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
      null = false;
      vars = T.Vars.empty;
      codes = T.Codes.comp x.codes;
    }

  let union x y =
    {
      eof = x.eof || y.eof;
      call = T.Vars.union x.call y.call;
      return = T.Vars.union x.return y.return;
      null = x.null || y.null;
      vars = T.Vars.union x.vars y.vars;
      codes = T.Codes.union x.codes y.codes;
    }

  let inter x y =
    {
      eof = x.eof && y.eof;
      call = T.Vars.inter x.call y.call;
      return = T.Vars.inter x.return y.return;
      null = x.null && y.null;
      vars = T.Vars.inter x.vars y.vars;
      codes = T.Codes.inter x.codes y.codes;
    }

  let diff x y =
    {
      eof = not (Bool.imp x.eof y.eof);
      call = T.Vars.diff x.call y.call;
      return = T.Vars.diff x.return y.return;
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

  val empty: t
  val add_multiple: key -> values -> t -> t
  val singleton_multiple: key -> values -> t
  val union: t -> t -> t
  val find_multiple: key -> t -> values
  val find_multiple_or_empty: key -> t -> values
end

module Lits_multimap(S: SET): MULTIMAP with type key = Lits.t and type values = S.t = struct
  module V = Multimap.Make1(T.Var_to)(S)
  module C = Refine.Map(T.Codes)(S)

  type key = Lits.t
  type values = S.t
  type t = {eof: S.t; call: V.t; return: V.t; null: S.t; vars: V.t; codes: C.t}
  [@@deriving eq, ord]

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
      codes = C.add k.codes vs C.empty;
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

  (*let add_seq_multiple s t =
    Seq.fold_left (fun t (k, v) -> add_multiple k v t) t s

  let of_seq_multiple s = 
    add_seq_multiple s empty*)
end

module R = Re.Abstract
module R' = Re.Porcelan(Re.Abstract)
module Rhs = Re.Concrete(T.Vars)(Lits)
module Rhs_to = Balanced_binary_tree.Map.Size(Rhs)


module Items = struct
  include Multimap.Make2(T.Labeled_var_to)(T.States)
  let pp = T.Labeled_var_to.pp T.States.pp
end
type items = Items.t

module Labels = struct
  type t = {items: Items.t; tail: Rhs.t; started: T.Labeled_vars.t; predictions: T.Labeled_vars.t; matches: T.Labeled_vars.t}

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
end

module Enhanced_vars = Multimap.Make2(T.State_to)(T.Vars)

module Enhanced_lits: sig
  include Re.LITS with type t = Lits.t T.State_to.t and type vars = Enhanced_vars.t
  val singleton: T.State.t -> Lits.t -> t
  val strip: t -> Lits.t
  val filter: (T.State.t -> Lits.t -> bool) -> t -> t

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

  let empty = M.empty

  let add_multiple k vs x =
    T.State_to.fold (fun q ls -> M.add_multiple q (L.singleton_multiple ls vs)) x k

  let singleton_multiple k vs = add_multiple k vs empty

  let union = M.union

  let find_multiple k x =
    T.State_to.fold (fun q ls ->
        S.union (L.find_multiple ls (M.find_multiple q x)))
      S.empty k

  let find_multiple_or_empty k t =
      try find_multiple k t
      with Not_found -> S.empty

  let strip x =
    T.State_to.fold (Fun.const L.union) L.empty x
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
  Labels.{
    items = Items.singleton lhs q;
    tail = rhs;
    started = T.Labeled_vars.(if is_start then singleton lhs else empty);
    predictions = T.Labeled_vars.(if not is_dead then singleton lhs else empty);
    matches = T.Labeled_vars.(if is_final then singleton lhs else empty);
  }

let convert ~supply (lhs, rhs') =
  let module Gen = M.Gen(T.State_index(Rhs_to)) in
  (lhs, Gen.unfold ~supply ~merge:Lits.union (fun q rhs ->
       let is_start = rhs == rhs' 
       and is_final = R.is_nullable rhs
       and is_dead = R.is_nothing rhs in
       let next = Seq.filter_map (fun ls ->
           let d = Rhs.simplify @@ Rhs.derivative ls rhs in
           if R.is_nothing d 
           then None 
           else Some (ls, d))
           (refine @@ Rhs.first rhs)
       in
       (is_final, label rhs is_start is_final is_dead lhs q, next))
      rhs')

let convert_multiple ~supply ps =
  Seq.zip (Supply.split supply) ps
  |> Seq.map (fun (supply, p) -> convert ~supply p)

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
            t
            |> M.link ~merge:Lits.union (T.States.singleton from) (Acc.find_multiple x starts) (Lits.call @@ T.Vars.singleton x)
            (*|> M.link ~merge:Lits.union (Acc.find_multiple x finals) (T.States.singleton to_) (Lits.return (T.Vars.singleton x))*))
          t ls.Lits.vars)
        t (M.transitions t))
  }

module State_pairs_to = Balanced_binary_tree.Map.Size(T.State_pair)

module Labeled_var_acc = Multimap.Make2(T.Labeled_var_to)(T.States)

type to_start = Labeled_var_acc.t

let to_start t =
  M.states_labels t
  |> Seq.flat_map (fun (q, ls) ->
      Seq.map (fun lhs -> (lhs, q)) @@ T.Labeled_vars.to_seq ls.Labels.started)
  |> Labeled_var_acc.of_seq

let extract state t1 t2  =
  M'.unfold (fun _ (from1, from2) ->
      let next = Seq.product (M.adjacent from1 t1) (M.adjacent from2 t2)
                 |> Seq.filter_map (fun ((to1, c1), (to2, c2)) ->
                     let c = Lits.inter c1 c2 in
                     if Lits.is_empty c || T.Labeled_vars.disjoint (M.labels to1 t1).Labels.predictions (M.labels to2 t2).Labels.predictions
                     then None
                     else Some ((Enhanced_lits.singleton from1 c), (to1, to2), (to1, to2)))
      in
      (M.is_final from2 t2, Labels.union (M.labels from1 t1) (M.labels from2 t2), next))
    (state, M.start t2) (state, M.start t2)

let extract_multiple ps to_start t =
  ps
  |> Seq.flat_map (fun (lhs, rhs) ->
      T.States.to_seq (Labeled_var_acc.find_multiple lhs to_start)
      |> Seq.map (fun state -> (((state, lhs): Enhanced_labeled_var.t), extract state t rhs)))

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
    shift_lookahead: Enhanced_lits.t;
    reduce_lookahead: Enhanced_lits.t;
  }

let lookahead' lb r lv s q =
  let lhss = lb lv s q in
  {
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

let lookahead ps =
  let module M = Enhanced_analysis.First.M in
  let first_start =  Seq.fold_left (fun acc (lhs, _) -> let x = Enhanced_lits.of_vars @@ Enhanced_labeled_var.to_vars lhs in M.add_multiple x x acc) M.empty ps in
  lookahead' (lookback ps) (Enhanced_analysis.analyse ~first_start ps)

let lookahead_tokens tokens la lv s q =
  let x = la lv s q in
  Enhanced_lits.union x.shift_lookahead x.reduce_lookahead
  |> Enhanced_lits.filter (fun _ ls -> not @@ Lits.is_empty (Lits.inter ls (Lits.of_vars tokens)))

type enhanced_vars = Enhanced_vars.t
type enhanced_labeled_var = Enhanced_labeled_var.t

module Enhanced_labeld_var_to = Balanced_binary_tree.Map.Size(Enhanced_labeled_var)

(*let follow ps =
  let ps' = Seq.map (fun ((q, lv), rhs) -> (Enhanced_vars.singleton q (T.Labeled_var.var lv), rhs)) ps in
  let (module N) = Enhanced_analysis.Nullable.get in
  let module A = Enhanced_analysis.Nullable.A in
  let n = N.fixedpoint ps' Enhanced_analysis.Nullable.A.empty in
  let (module F) = Enhanced_analysis.First.get (fun lhs -> A.find_multiple_or_empty lhs n) in
  (*Seq.iter (fun (lhs, _) ->
      Fmt.pr "@[[%a::%b]@]" Enhanced_lits.pp (Enhanced_lits.of_vars lhs) (A.find_multiple (Enhanced_lits.of_vars lhs) n))
    ps';*)
  let module A = Enhanced_analysis.First.A in
  let f = F.fixedpoint ps' (Seq.fold_left (fun acc (lhs, _) -> A.add_multiple (Enhanced_lits.of_vars lhs) (Enhanced_lits.of_vars lhs) acc) A.empty ps') in
  (*Seq.iter (fun (lhs, _) ->
      Fmt.pr "@[[%a->%a]@]" Enhanced_lits.pp (Enhanced_lits.of_vars lhs) Enhanced_lits.pp (A.find_multiple (Enhanced_lits.of_vars lhs) f))
    ps';*)
  let n' = Enhanced_labeld_var_to.of_seq @@ N.extend_multiple ps n in
  let f' = Enhanced_labeld_var_to.of_seq @@ F.extend_multiple ps f in
  let to_lits =
      (fun (q, lv) -> Enhanced_lits.of_vars @@ Enhanced_vars.singleton q @@ T.Labeled_var.var lv) in
  let module A = Enhanced_analysis.Follow.A in
  let (module X) = Enhanced_analysis.Follow.get
      (fun lhs q -> (Enhanced_labeld_var_to.find lhs n') q)
      (fun lhs q -> (Enhanced_labeld_var_to.find lhs f') q)
      to_lits
  in
  let f = X.fixedpoint ps A.empty in
  (*Seq.iter (fun (lhs, _) ->
      Fmt.pr "@[[%a???%a]@]" Enhanced_lits.pp (to_lits lhs) Enhanced_lits.pp (A.find_multiple_or_empty (to_lits lhs) f))
    ps;*)
  (fun x -> A.find_multiple_or_empty x f)*)

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
                     if Lits.is_empty c || T.Labeled_vars.disjoint (M.labels to1 m1).Labels.predictions (M.labels to2 m2).Labels.predictions
                     then None
                     else Some (c, (to1, to2), (to1, to2)))
       in
       let adj' = 
         M.adjacent from2 m2
         |> Seq.filter_map (fun (s, ls) -> if Lits.is_call ls then Some (ls, (from1, s), (from1, s)) else None)
       in
       ((M.labels from2 m2).tail, Seq.(adj @ adj')))
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
  |> Seq.map (fun (s, ls) -> (ls.Labels.matches, T.States.singleton s))
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

module Noncannonical = Fa.Make

let noncannonical ~supply token_lookahead t =
  let module Gen = Noncannonical.Gen(T.State_index(States_to)) in
  Gen.unfold ~supply ~merge:Lits.union (fun _ from ->
      let module R = Refine.Map(Lits)(T.States) in
      let nc =
        T.States.to_seq from
        |> Seq.flat_map (fun s ->
            (M.labels s t).Labels.items
            |> Items.to_seq
            |> Seq.map (fun (lv, q) -> token_lookahead lv s q))
        |> Seq.fold_left Enhanced_lits.union Enhanced_lits.empty
      in
      let nc = Enhanced_lits.to_seq nc
               |> Seq.flat_map (fun (s, ls) -> (Seq.filter_map (fun (p, ls') ->
                   if Lits.subset ls' ls
                   then Some (p, ls')
                   else None)
                   (M.adjacent s t)))
      in
      let next =
        Seq.append (M.adjacent_multiple from t) nc
        |> Seq.map (fun (s, ls) -> (ls, T.States.singleton s))
        |> R.refine
        |> R.partitions
      in
      (M.is_final_multiple from t, from, next))
    (M.start_multiple t)

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

type rtn =
  {
    start: T.State.t;
    final: T.States.t;
    graph: (Rhs.t T.Labeled_var_to.t, Lits.t) G.t
  }

let rtn_to_string t =
  let string_of_labels l =
    Fmt.to_to_string (T.Labeled_var_to.pp Rhs.pp) l
  in
  let string_of_lits = Fmt.to_to_string Lits.pp in
  let node q l last labels = Dot.(node (T.State.to_id q) ~attrs:[
      "label" => String l;
      "peripheries" => String (if last then "2" else "1");
      "xlabel" => String labels
    ])
  and edge q0 q1 l = Dot.(edge Directed (T.State.to_id q0) (T.State.to_id q1) ~attrs:[
      "label" => String l
    ])
  and start_edges =
    Seq.return @@
    Dot.(edge Directed (String "start") (T.State.to_id t.start))
  and start_node = Seq.return
      Dot.(node (String "start") ~attrs:[
          "style" => String "invis"
        ])
in
  let nodes = Seq.map (fun (q, l) -> 
      (node q (Fmt.to_to_string T.State.pp q) (T.States.mem q t.final) (string_of_labels l)))
      (G.labeled_vertices t.graph) 
  in
  let edges = Seq.map (fun (q0, q1, ss) ->
      edge q0 q1 (string_of_lits ss))
      (G.labeled_edges t.graph) 
  in
  Dot.string_of_graph
  Dot.(Digraph, "g", List.of_seq @@ Seq.(nodes @ edges @ start_node @ start_edges))

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
