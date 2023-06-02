open Te_bot
open! Prelude 
module T = Types
module G = T.State_graph

let pp_if b pp ppf  =
  if b then Fmt.pf ppf "%a@ " pp else Fmt.nop ppf

module Lits: sig
  type t = {eof: bool; call: T.Vars.t; return: T.Vars.t; null: bool; vars: T.Vars.t; codes: T.Codes.t}
  include Re.LITS with type t := t
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

module R = Re.Porcelan(Re.Abstract)
module Rhs = Re.Concrete(Lits)
module Rhs_to = Balanced_binary_tree.Map.Size(Rhs)

module Items' = Multimap.Make(T.Labeled_var_to)
module Items = struct 
  include Items'.L2(T.States)
  let pp = T.Labeled_var_to.pp T.States.pp
end

type items = Items.t

module Labels = struct
  type t = {items: Items.t; tail: Rhs.t; started: T.Labeled_vars.t; predictions: T.Labeled_vars.t; matches: T.Labeled_vars.t}
  [@@deriving eq, ord]

  let pp ppf x =
    Fmt.pf ppf "@[%a%a%a%a%a@]"
      (pp_if (not @@ Rhs.is_nothing x.tail) (fun ppf -> Fmt.pf ppf "%a" Rhs.pp)) x.tail
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

  (*let inter x y =
    {
      predictions = T.Labeled_vars.inter x.predictions y.predictions;
      matches = T.Labeled_vars.inter x.matches y.matches;
    }*)
end

module Enhanced_lits: sig
  include Re.LITS with type t = Lits.t T.State_to.t
  val singleton: T.State.t -> Lits.t -> t
  val strip: t -> Lits.t
end = struct
  type t = Lits.t T.State_to.t
  [@@deriving eq, ord]

  let pp ppf x =
    Fmt.pf ppf "@[%a@]" (T.State_to.pp Lits.pp) x

  let subset x y =
    T.State_to.subset Lits.equal x y

  let strip t =
    T.State_to.fold (fun _ ls acc -> Lits.union ls acc) Lits.empty t

  let singleton q ls =
    T.State_to.singleton q ls
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

module M = Fa.Make(Labels)(Lits)

module M' = Fa.Make(Labels)(Enhanced_lits)

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
      if Rhs.is_nullable (Production.rhs p)
      then Some (T.Labeled_var.var @@ Production.lhs p)
      else None)
      ps
  in
  T.Vars.of_seq @@ Seq.filter_map (fun p ->
      if Rhs.is_nullable' (Lits.is_nullable (fun v -> T.Vars.mem v ns'))  (Production.rhs p)
      then Some (T.Labeled_var.var @@ Production.lhs p)
      else None)
    ps

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
       and is_final = Rhs.is_nullable rhs
       and is_dead = Rhs.is_nothing rhs in
       let next = Seq.filter_map (fun ls ->
           let d = Rhs.simplify @@ Rhs.derivative ls rhs in
           if Rhs.is_nothing d 
           then None 
           else Some (ls, d))
           (refine @@ Rhs.head rhs)
       in
       (is_final, label rhs is_start is_final is_dead lhs q, next))
      rhs')

let convert_multiple ~supply ps =
  Seq.zip (Supply.split supply) ps
  |> Seq.map (fun (supply, p) -> convert ~supply p)

module Acc' = Multimap.Make(T.Var_to)
module Acc = Acc'.L2(T.States)

module Enhanced_var = struct
  type t = T.State.t * T.Var.t
  [@@deriving ord]
end
module Enhanced_var_to = Balanced_binary_tree.Map.Size(Enhanced_var)
module Acc2' = Multimap.Make(Enhanced_var_to)
module Acc2 = Acc2'.L2(T.States)

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

module P = Fa.Make(T.State_pairs)(Lits)

module Labeled_var_acc' = Multimap.Make(T.Labeled_var_to) 
module Labeled_var_acc = Labeled_var_acc'.L2(T.States)

type to_start = Labeled_var_acc.t

let to_start t =
  M.states_labels t
  |> Seq.flat_map (fun (q, ls) -> 
      Seq.map (fun lhs -> (lhs, q)) @@ T.Labeled_vars.to_seq ls.Labels.started)
  |> Labeled_var_acc.of_seq

let extract ~supply state t1 t2  =
  let module Gen = M'.Gen(T.State_index(State_pairs_to)) in
  Gen.unfold ~supply (fun _ (from1, from2) ->
      let next = Seq.product (M.adjacent from1 t1) (M.adjacent from2 t2)
                 |> Seq.filter_map (fun ((to1, c1), (to2, c2)) ->
                     let c = Lits.inter c1 c2 in
                     if Lits.is_empty c || T.Labeled_vars.disjoint (M.labels to1 t1).predictions (M.labels to2 t2).predictions
                     then None
                     else Some ((Enhanced_lits.singleton from1 c), (to1, to2)))
      in
      (M.is_final from2 t2, Labels.union (M.labels from1 t1) (M.labels from2 t2), next))
    (state, M.start t2)

let extract_multiple ~supply ps to_start t =
  Seq.zip (Supply.split supply) ps
  |> Seq.flat_map (fun (supply, (lhs, rhs)) ->
      Seq.zip (Supply.split supply) (T.States.to_seq (Labeled_var_acc.find_multiple lhs to_start))
      |> Seq.map (fun (supply, state) -> ((lhs, state), extract ~supply state t rhs)))

let strip ~supply t =
  let module Gen = M.Gen(T.State_index(T.State_to)) in
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
   PG.unfold' (fun _ _ (from1, from2) ->
       let adj = Seq.product (M.adjacent from1 m1) (M.adjacent from2 m2)
                 |> Seq.filter_map (fun ((to1, c1), (to2, c2)) ->
                     let c = Lits.inter c1 c2 in
                     if Lits.is_empty c || T.Labeled_vars.disjoint (M.labels to1 m1).predictions (M.labels to2 m2).predictions
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

module Kleene = Re.Kleene(Lits)(T.State_pair_graph)

let rev_solve = Kleene.rev_solve 

let right_context (start, g) =
  fun s q -> R.concat (PG.vertex_label (s, q) g) (PG.edge_label start (s, q) g)

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

let partition (t: M.Start.single M.t) = 
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

let lookahead rc g =
  {
    start = M.start g;
    final = M.final g;
    graph = G.labeled_vertices_map (fun s ls ->
        ls.Labels.items
        |> Items.to_seq
        |> Seq.filter_map (fun (lhs, q) ->
            if T.Labeled_vars.mem lhs ls.Labels.matches
            then Some (lhs, rc s q)
            else None)
        |> T.Labeled_var_to.of_seq) g.graph
  }

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


