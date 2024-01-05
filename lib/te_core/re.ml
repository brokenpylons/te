open Te_bot
open! Prelude 

module Abstract = struct
  type 'ls t =
    | Nothing
    | Null
    | Any
    | Lits of 'ls
    | Concat of bool * bool * 'ls t * 'ls t
    | Union of bool * bool * 'ls t * 'ls t
    | Repeat of bool * bool * 'ls t * int
    | Star of 'ls t
    | Comp of bool * bool * 'ls t
  [@@deriving eq, ord]

  let pp pp_lits =
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
    in go

  let rec reverse = function
    | Concat (m, h, x, y) -> Concat (m, h, reverse y, reverse x)
    | Union (m, h, x, y) -> Union (m, h, reverse x, reverse y)
    | Repeat (m, h, x, i) -> Repeat (m, h, reverse x, i)
    | Star x -> Star (reverse x)
    | Comp (m, h, x) -> Comp (m, h, reverse x)
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

  let comp_nothing =
    Comp (true, false, Nothing)

  let comp_null =
    Comp (false, false, Null)

  let comp_any =
    Comp (true, false, Any)

  exception Undefined

  (*let rec merge cons' xs ys =
    match xs (), ys () with
    | Seq.Cons (x, xs), Seq.Cons (y, ys) ->
      cons' x y (merge cons' xs ys)
    | Seq.Cons (x, xs), Seq.Nil
    | Seq.Nil, Seq.Cons (x, xs) -> Seq.cons x xs
    | Seq.Nil, Seq.Nil -> Seq.empty*)

  (*let rec product cons' ~cmp xs ys =
    match xs (), ys () with
    | Seq.Cons (x, xs), Seq.Cons (y, ys) ->



      cons' x y (merge cons' xs ys)
    | Seq.Cons (x, xs), Seq.Nil
    | Seq.Nil, Seq.Cons (x, xs) -> Seq.cons x xs
    | Seq.Nil, Seq.Nil -> Seq.empty*)


  let rec insert cmp r1 t =
    (match Seq.uncons r1 with
     | None -> t
     | Some (x, r1) ->
       (match Seq.uncons t with
        | None -> Seq.return (Seq.cons x r1)
        | Some (r2, t) ->
          (match Seq.uncons r2 with
           | None -> t
           | Some (y, r2) ->
             let c = Int.compare (Seq.length x) (Seq.length y) in
               (if c <= 0 then
                  Seq.cons (Seq.cons x r1) (Seq.cons (Seq.cons y r2) t)
               else
                 Seq.cons (Seq.cons y r2) (insert cmp (Seq.cons x r1) t)))))


  let rec fair_concat cmp t =
    match Seq.uncons t with
    | None -> Seq.empty
    | Some (r1, t) ->
      (match Seq.uncons r1 with
       | None -> fair_concat cmp t
       | Some (x11, r1) ->
         (match Seq.uncons r1 with
          | None -> fun () -> Seq.Cons (x11, fair_concat cmp t)
          | Some (x12, r1) ->
            (match Seq.uncons t with
             | None -> Seq.cons x11 (Seq.cons x12 r1)
             | Some (r2, t) ->
               (match Seq.uncons r2 with
                | None -> fair_concat cmp (Seq.cons (Seq.cons x11 (Seq.cons x12 r1)) t)
                | Some (x21, r2) ->
                  let t = insert cmp (Seq.cons x12 r1) (Seq.cons (Seq.cons x21 r2) t) in
                  fun () -> Seq.Cons (x11, fair_concat cmp t)))))


  let merge cmp xs ys =
    match Seq.uncons xs, Seq.uncons ys with
    | Some (x, _), Some (y, _) ->
      let c = Int.compare (Seq.length x) (Seq.length y) in
      if c <= 0 then
        fair_concat cmp
          (Seq.cons xs (Seq.cons ys Seq.empty))
      else
        fair_concat cmp
          (Seq.cons ys (Seq.cons xs Seq.empty))
    | Some (_, _), None -> xs
    | None, Some (_, _) -> ys
    | None, None -> Seq.empty

  let to_seq ~cmp (f: 'a -> 'b Seq.t) =
    let rec go: 'a t -> 'b Seq.t Seq.t = function
      | Nothing -> Seq.empty
      | Null -> Seq.return Seq.empty
      | Any -> raise Undefined
      | Lits ls -> Seq.map Seq.return (f ls)
      | Concat (_, _, x, y) ->
        let xs = go x in
        let ys = go y in
        fair_concat cmp
        (Seq.map (fun x ->
            Seq.map (fun y -> Seq.append x y) ys)
          xs)
      | Union (_, _, x, y) ->
        merge cmp (go x) (go y)
      | Repeat (_, _, _x, _i) ->
        raise Undefined
      (*Seq.map (uncurry (@))
        @@ Seq.take i
        @@ Seq.repeat (to_seq f x)*)
      | Star x ->
        let xs = go x in
        fair_concat cmp
        (Seq.unfold (fun acc ->
            Some (acc, fair_concat cmp
              (Seq.map (fun x ->
                   Seq.map (fun y -> Seq.append x y) acc)
                  xs)))
          (Seq.return Seq.empty))
      | Comp _ -> raise Undefined
    in go

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

module type LITS = sig
  type t
  val pp: t Fmt.t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val subset: t -> t -> bool
end

module type CONCRETE = sig
  type lits
  type t = lits Abstract.t

  val pp: t Fmt.t
  val equal: t -> t -> bool
  val compare: t -> t -> int

  val derivative: lits -> t -> t

  val first': (t -> bool) -> t -> lits Seq.t
  val first: t -> lits Seq.t
  val simplify: t -> t
end

module Concrete(Lits: LITS):
  CONCRETE with type lits = Lits.t = struct
  open Abstract
  type lits = Lits.t
  type t = Lits.t Abstract.t
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
    in go

  let first = 
    first' is_nullable

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
end

module Kleene(Lits: LITS)(G: Graph.S) = struct
  open Abstract
  open Concrete(Lits)

  let labels s p g =
    try G.edge_label s p g with Not_found -> nothing

  let solve' f g =
    let vertices = Seq.memoize @@ G.vertices g in
    Seq.fold_left (fun g p ->
        let loop = labels p p g in
        Seq.fold_left (fun g (s, q) ->
            let l = simplify (f s p q loop g + labels s q g) in
            if is_nothing l then g else G.connect s q l g)
          g (Seq.product vertices vertices))
      g vertices

  let solve g =
    solve' (fun s p q loop g ->
        labels s p g * star loop * labels p q g) g

  let rev_solve g =
    solve' (fun s p q loop g ->
        labels p q g * star loop * labels s p g) g
end
