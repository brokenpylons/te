open Te_bot
open! Prelude 

(* Core set of regular expressions *)
module type OP = sig
  type (+_, +_) t
  val pp: 'vs Fmt.t -> 'ls Fmt.t -> ('vs, 'ls) t Fmt.t

  val nothing: _ t
  val null: _ t
  val any: _ t
  val comp_nothing: _ t
  val comp_null: _ t
  val comp_any: _ t
  val lits: 'ls -> ('vs, 'ls) t
  val concat: ('vs, 'ls) t -> ('vs, 'ls) t -> ('vs, 'ls) t
  val union: ('vs, 'ls) t -> ('vs, 'ls) t -> ('vs, 'ls) t
  val repeat: ('vs, 'ls) t -> int -> ('vs, 'ls) t
  val star: ('vs, 'ls) t -> ('vs, 'ls) t
  val comp: ('vs, 'ls) t -> ('vs, 'ls) t
end

module Abstract = struct
  type ('vs, 'ls) t =
    | Nothing
    | Null
    | Any
    | Lits of 'ls
    | Concat of bool * bool * ('vs, 'ls) t * ('vs, 'ls) t
    | Union of bool * bool * ('vs, 'ls) t * ('vs, 'ls) t
    | Repeat of bool * bool * ('vs, 'ls) t * int
    | Star of ('vs, 'ls) t
    | Comp of bool * bool * ('vs, 'ls) t
    | Fix of bool * bool * 'vs * ('vs, 'ls) t
  [@@deriving eq, ord]

  let pp pp_vars pp_lits =
    let rec go ppf = function
      | Nothing -> Fmt.string ppf "∅"
      | Null -> Fmt.string ppf "ε"
      | Any -> Fmt.string ppf "."
      | Lits ls -> pp_lits ppf ls
      | Concat (_, _, x, y) -> Fmt.pf ppf "@[(@[%a@]@ . @[%a@])@]" go x go y
      | Union (_, _, x, y) -> Fmt.pf ppf "@[(@[%a@]@,|@,@[%a@])@]" go x go y   
      | Repeat (_, _, x, i) -> Fmt.pf ppf "@[%a@,@[{%i}@]@]" go x i
      | Star x -> Fmt.pf ppf "@[(@[%a@]){*}@]" go x
      | Comp (_, _, x) -> Fmt.pf ppf "@[¬(@[%a@])@]" go x
      | Fix (_, _, vs, x) -> Fmt.pf ppf "@[(@[%a@]){*%a}@]" go x pp_vars vs
    in go

  let is_nullable_concat x y =
    x && y

  let is_nullable_union x y =
    x || y

  let is_nullable_repeat x i = 
    match i with
    | 0 -> true
    | i when i > 0 -> x
    | _ -> assert false

  let is_nullable_comp x =
    not x

  let is_nothing_concat x y =
    x || y

  let is_nothing_union x y =
    x && y

  let is_nothing_repeat x i = 
    match i with
    | 0 -> false
    | i when i > 0 -> x
    | _ -> assert false

  let is_nothing_comp x =
    not x

  let is_nullable = function
    | Nothing -> false
    | Null -> true
    | Any -> false
    | Lits _ -> false
    | Concat (m, _, _, _) -> m
    | Union (m, _, _, _) -> m
    | Repeat (m, _, _, _) -> m
    | Star _ -> true
    | Comp (m, _, _) -> m
    | Fix (m, _, _, _) -> m

  let is_nullable' n =
    let rec go = function
      | Nothing -> false
      | Null -> true
      | Any -> false
      | Lits ls -> n ls
      | Concat (_, _, x, y) -> is_nullable_concat (go x) (go y)
      | Union (_, _, x, y) -> is_nullable_union (go x) (go y)
      | Repeat (_, _, x, i) -> is_nullable_repeat (go x) i
      | Star _ -> true
      | Comp (_, _, x) -> is_nullable_comp (go x)
      | Fix (_, _, _, x) -> go x
    in go

  let is_nothing = function
    | Nothing -> true
    | Null -> false
    | Any -> false
    | Lits _ -> false
    | Concat (_, h, _, _) -> h
    | Union (_, h, _, _) -> h
    | Repeat (_, h, _, _) -> h
    | Star _ -> false
    | Comp (_, h, _) -> h
    | Fix (_, h, _, _) -> h

  (*let lower_bound_comp = function
    | Size.Top -> Size.bot
    | Size.Bot -> Size.of_int 0
    | Size.Finite 0 -> Size.of_int 1
    | Size.Finite _ -> Size.of_int 0

  let lower_bound n b =
    let rec go = function
      | Nothing -> Size.bot
      | Null -> Size.of_int 0
      | Any -> Size.of_int 1
      | Lits ls -> b ls
      | Concat (_, _, x, y) when is_nullable' n x -> Size.(go x + go y)
      | Concat (_, _, x, _) -> go x
      | Union (_, _, x, y) -> Size.min (go x) (go y)
      | Repeat (_, _, x, _) -> go x
      | Star _ -> Size.of_int 0
      | Comp (_, _, x) -> lower_bound_comp @@ go x
    in go*)

  let is_infinite =
    let rec go = function
      | Nothing -> true
      | Null -> false
      | Any -> false
      | Lits _ -> false
      | Concat (_, _, x, y) when is_nullable x -> go x || go y
      | Concat (_, _, x, _) -> go x
      | Union (_, _, x, y) -> go x || go y
      | Repeat (_, _, x, _) -> go x
      | Star _ -> true
      | Comp (_, _, x) -> not (go x)
      | Fix (_, _, _, _) -> true
    in go

  let rec reverse = function
    | Concat (m, h, x, y) -> Concat (m, h, reverse y, reverse x)
    | Union (m, h, x, y) -> Union (m, h, reverse x, reverse y)
    | Repeat (m, h, x, i) -> Repeat (m, h, reverse x, i)
    | Star x -> Star (reverse x)
    | Comp (m, h, x) -> Comp (m, h, reverse x)
    | Fix (m, h, vs, x) -> Fix (m, h, vs, reverse x)
    | x -> x

  let nothing =
    Nothing

  let null =
    Null

  let any =
    Any

  let lits s = 
    Lits s

  let concat x y =
    Concat (is_nullable_concat (is_nullable x) (is_nullable y), is_nothing_concat (is_nothing x) (is_nothing y), x, y)

  let union x y =
    Union (is_nullable_union (is_nullable x) (is_nullable y), is_nothing_union (is_nothing x) (is_nothing y), x, y)

  let repeat x i =
    Repeat (is_nullable_repeat (is_nullable x) i, is_nothing_repeat (is_nullable x) i, x, i)

  let star x =
    Star x

  let comp x =
    Comp (is_nullable_comp (is_nullable x), is_nothing_comp (is_nothing x), x)

  let fix vt x = 
    Fix (is_nullable x, is_nothing x, vt, x)

  let comp_nothing =
    Comp (true, false, Nothing)

  let comp_null =
    Comp (false, false, Null)

  let comp_any =
    Comp (true, false, Any)
end

module type VARS = sig
  type t
  val pp: t Fmt.t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val subset: t -> t -> bool

  val empty: t
  val is_empty: t -> bool
  val inter: t -> t -> t
  val diff: t -> t -> t
  val union: t -> t -> t
end

module type LITS = sig
  type t
  type vars
  val pp: t Fmt.t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val subset: t -> t -> bool
  val to_vars: t -> vars
  val of_vars: vars -> t
end

module type CONCRETE = sig
  type vars
  type lits
  type t = (vars, lits) Abstract.t

  val pp: t Fmt.t
  val equal: t -> t -> bool
  val compare: t -> t -> int

  val derivative: lits -> t -> t
  val derivative': vars -> (vars -> t) -> (lits -> bool) -> lits -> t -> t

  val first': (t -> bool) -> t -> lits Seq.t
  val first: t -> lits Seq.t
  (*val occur: t -> lits Seq.t*)
  val free: t -> vars
  val simplify: t -> t

  val to_seq: (lits -> 'a Seq.t) -> t -> 'a Seq.t Seq.t
end

module Concrete(Vars: VARS)(Lits: LITS with type vars = Vars.t): 
  CONCRETE with type lits = Lits.t and type vars = Vars.t = struct
  open Abstract
  type vars = Vars.t
  type lits = Lits.t
  type t = (Vars.t, Lits.t) Abstract.t
  [@@deriving eq, ord, show]

  let first' n = 
    let rec go = function
      | Nothing -> Seq.empty
      | Null -> Seq.empty
      | Any -> Seq.empty
      | Lits ls -> Seq.return ls
      | Concat (_, _, x, y) when n x -> Seq.append (go x) (go y)
      | Concat (_, _, x, _) -> go x
      | Union (_, _, x, y) -> Seq.append (go x) (go y)
      | Repeat (_, _, x, _) -> go x
      | Star x -> go x
      | Comp (_, _, x) -> go x
      | Fix (_, _, _, x) -> go x
    in go

  let first = 
    first' is_nullable

  let rec free = function
    | Nothing -> Vars.empty
    | Null -> Vars.empty
    | Any -> Vars.empty
    | Lits ls -> Lits.to_vars ls
    | Concat (_, _, x, y) -> Vars.union (free x) (free y)
    | Union (_, _, x, y) -> Vars.union (free x) (free y)
    | Repeat (_, _, x, _) -> free x
    | Star x -> free x
    | Comp (_, _, x) -> free x
    | Fix (_, _, vs, x) -> Vars.diff (free x) vs

  let rec simplify = function
    | Union (_, _, x, Union (_, _, y, z)) -> simplify (union (simplify (union x y)) (simplify z))
    | Union (m, h, x, y) ->
      (match simplify x, simplify y with
       | Nothing, x -> x
       | x, Nothing -> x
       | Comp (_, _, Nothing), _ -> comp_nothing
       | _, Comp (_, _, Nothing) -> comp_nothing
       | x, y when equal x y -> x
       | x, y when compare x y > 0 -> Union (m, h, y, x)
       | x, y -> Union (m, h, x, y))

    | Concat (_, _, x, Concat (_, _, y, z)) -> simplify (concat (simplify (concat x y)) (simplify z))
    | Concat (m, h, x, y) -> 
      (match simplify x, simplify y with
       (*| Union (_, _, Null, x), Star y when equal x y -> simplify (Star x)
       | Star x, Union (_, _, Null, y) when equal x y -> simplify (Star x)*)
       | Nothing, _ -> nothing
       | _, Nothing -> nothing
       | Null, x -> x
       | x, Null -> x
       | Comp (_, _, Nothing), Comp (_, _, Nothing) -> comp_nothing
       | Comp (_, _, Null), Comp (_, _, Null) -> comp_null
       | x, y -> Concat (m, h, x, y))

    | Repeat (_, _, _, 0) -> null
    | Repeat (m, h, x, i) -> 
      (match simplify x with
       | Nothing -> nothing
       | Null -> nothing
       | Comp (_, _, Nothing) -> comp_nothing
       | Comp (_, _, Null) -> comp_null
       | x -> Repeat (m, h, x, i))

    | Star x -> 
      (match simplify x with
       (*| Union (_, _, Null, x) -> simplify (Star x)*)
       | Null -> null
       | Nothing -> null
       | Comp (_, _, Nothing) -> comp_nothing
       | Comp (_, _, Null) -> comp_nothing
       | Star x -> x
       | x -> Star x)

    | Comp (m, h, x) -> 
      (match simplify x with
       | Comp (_, _, x) -> x
       | x -> Comp (m, h, x))

    | Fix (_, _, vs, x) -> 
      (match simplify x with
       | x when not (Vars.subset vs (free x)) -> x
       | Fix (_, _, vs', x) when Vars.equal vs vs' -> fix vs x
       | x -> fix vs x)

    | x -> x

  let rec derivative s = function
    | Nothing -> nothing
    | Null -> nothing
    | Any -> null
    | Lits ls -> 
      if Lits.subset s ls
      then null
      else nothing
    | Concat (_, _, x, y) when is_nullable x -> union (concat (derivative s x) y) (derivative s y)
    | Concat (_, _, x, y) -> concat (derivative s x) y
    | Union (_, _, x, y) -> union (derivative s x) (derivative s y)
    | Repeat (_, _, x, i) -> 
      (match i with
       | 0 -> nothing
       | i -> concat (derivative s x) (repeat x (pred i)))
    | Star x ->
      concat (derivative s x) (star x)
    | Comp (_, _, x) ->
      comp (derivative s x)
    | Fix (_, _, _, _) -> nothing

  let rec derivative' b p n s = function
    | Nothing -> nothing
    | Null -> nothing
    | Any -> null
    | Lits ls -> 
      let vs = Lits.to_vars ls in
      let b' = Vars.inter vs b 
      and f = Vars.diff vs b in
      union (if Lits.subset s ls then null else nothing)
        (union (if Vars.is_empty b' then nothing else lits (Lits.of_vars b')) (if Vars.is_empty f then nothing else (derivative' b p n s (fix f (p f)))))
    | Concat (_, _, x, y) when is_nullable' n x -> union (concat (derivative' b p n s x) y) (derivative' b p n s y)
    | Concat (_, _, x, y) -> concat (derivative' b p n s x) y
    | Union (_, _, x, y) -> union (derivative' b p n s x) (derivative' b p n s y)
    | Repeat (_, _, x, i) -> 
      (match i with
       | 0 -> nothing
       | i -> concat (derivative' b p n s x) (repeat x (pred i)))
    | Star x ->
      concat (derivative' b p n s x) (star x)
    | Comp (_, _, x) ->
      comp (derivative' b p n s x)
    | Fix (_, _, vs, y) ->
      fix vs (derivative' (Vars.union vs b) p (fun vs' -> if Lits.subset (Lits.of_vars vs) vs' then is_nullable y else n vs') s y)

  exception Undefined

  let rec to_seq f = function
    | Nothing -> Seq.empty
    | Null -> Seq.return Seq.empty
    | Any -> raise Undefined
    | Lits ls -> Seq.return (f ls)
    | Concat (_, _, x, y) ->
      Seq.map (uncurry Seq.append)
      @@ Seq.product (to_seq f x) (to_seq f y)
    | Union (_, _, x, y) ->
      Seq.append (to_seq f x) (to_seq f y)
    | Repeat (_, _, x, i) ->
      Seq.concat
      @@ Seq.take i
      @@ Seq.repeat (to_seq f x)
    | Star _ ->
      raise Undefined
      (*Seq.concat
      @@ Seq.repeat (to_seq f x)*)
    | Comp _ -> raise Undefined
    | Fix _ -> raise Undefined
end

module Porcelan(C: OP) = struct
  include C

  let comp_lits x =
    comp (lits x)

  let comp_concat x y =
    comp (concat (comp x) (comp y))

  let comp_union x y =
    comp (union (comp x) (comp y))

  let inter = comp_union

  let comp_repeat x i =
    comp (repeat (comp x) i)

  let comp_star x =
    comp x

  let opt x =
    union null x

  let comp_opt x =
    comp (opt (comp x))

  let force = comp_opt 

  let diff x y =
    comp (union (comp x) y)

  let comp_diff x y =
    comp (union x (comp y))

  let wildcard = 
    any

  let interval x i j =
    concat (repeat x i) (repeat (opt x) j)

  let plus x =
    concat x (star x)

  let not = comp

  let universe =
    comp_nothing

  let nonnull =
    comp_null

  let ( * ) x y = concat x y
  let (+) x y = union x y
  let (-) x y = diff x y
  let (&) x y = inter x y
  let (|..) x i = repeat x i
  let (|...) x (i, j) = interval x i j
end

module Kleene(Vars: VARS)(Lits: LITS with type vars = Vars.t)(G: Graph.S) = struct
  open Abstract
  open Concrete(Vars)(Lits)
  open Porcelan(Abstract)

  let labels s p g =
    try G.edge_label s p g with Not_found -> nothing

  (*let initial =
      G.labeled_edges_map (fun _ _ ls -> lits ls) g
      |>
    in*)

  (*let initial = Seq.fold_left (fun g p ->
      G.connect p p (union (labels p p g) null) g)
      g vertices
    in*)

  (*Seq.product (T.States.to_seq @@ start_multiple m) (T.States.to_seq @@ final m)
    |> Seq.fold_left (fun e (s, q) -> 
      R.union (lits' s q g) e)
    R.nothing*)

  let solve g =
    let vertices = Seq.memoize @@ G.vertices g in
    Seq.fold_left (fun g p ->
        let loop = labels p p g in
        Seq.fold_left (fun g (s, q) ->
            let l = simplify (labels s p g * star loop * labels p q g + labels s q g) in
            if is_nothing l then g else G.connect s q l g)
          g (Seq.product vertices vertices))
      g vertices 

  let rev_solve g =
    let vertices = Seq.memoize @@ G.vertices g in
    Seq.fold_left (fun g p ->
        let loop = labels p p g in
        Seq.fold_left (fun g (s, q) ->
            let l = simplify (labels p q g * star loop * labels s p g + labels s q g) in
            if is_nothing l then g else G.connect s q l g)
          g (Seq.product vertices vertices))
      g vertices 
end
