open Te_bot
open! Prelude
module T = Types

module Abstract = struct
  type 'ls t =
    | Lits of 'ls
    | Any
    | Null
    | Nothing
    | Concat of bool * bool * 'ls t * 'ls t
    | Repeat of bool * bool * 'ls t * int
    | Star of 'ls t
    | Comp of bool * 'ls t
    | Union of bool * bool * 'ls t * 'ls t
  [@@deriving eq, ord]

  let pp pp_lits =
    let rec go ppf = function
      | Nothing -> Fmt.string ppf "∅"
      | Null -> Fmt.string ppf "ε"
      | Any -> Fmt.string ppf "."
      | Lits ls -> pp_lits ppf ls
      | Concat (_, _, x, y) -> Fmt.pf ppf "@[(@[%a@]@,·@,@[%a@])@]" go x go y
      | Union (_, _, x, y) -> Fmt.pf ppf "@[(@[%a@]@,|@,@[%a@])@]" go x go y
      | Repeat (_, _, x, i) -> Fmt.pf ppf "@[%a@,@[{%i}@]@]" go x i
      | Star x -> Fmt.pf ppf "@[(@[%a@]){*}@]" go x
      | Comp (_, x) -> Fmt.pf ppf "@[¬(@[%a@])@]" go x
    in go

  let map f =
    let rec go = function
      | Nothing -> Nothing
      | Null -> Null
      | Any -> Any
      | Lits ls -> Lits (f ls)
      | Concat (m, h, x, y) -> Concat (m, h, go x, go y)
      | Union (m, h, x, y) -> Union (m, h, go x, go y)
      | Repeat (m, h, x, i) -> Repeat (m, h, go x, i)
      | Star x -> Star (go x)
      | Comp (m, x) -> Comp (m, go x)
    in go

  let flat_map f =
    let rec go = function
      | Nothing -> Nothing
      | Null -> Null
      | Any -> Any
      | Lits ls -> f ls
      | Concat (m, h, x, y) -> Concat (m, h, go x, go y)
      | Union (m, h, x, y) -> Union (m, h, go x, go y)
      | Repeat (m, h, x, i) -> Repeat (m, h, go x, i)
      | Star x -> Star (go x)
      | Comp (m, x) -> Comp (m, go x)
    in go

  module Is_nullable = struct
    let concat x y =
      x && y

    let union x y =
      x || y

    let repeat x i = 
      match i with
      | 0 -> true
      | i when i > 0 -> x
      | _ -> assert false

    let comp x =
      not x
  end

  module Is_nothing = struct
    let concat x y =
      x || y

    let union x y =
      x && y

    let repeat x i = 
      match i with
      | 0 -> false
      | i when i > 0 -> x
      | _ -> assert false
  end

  let is_nullable = function
    | Nothing -> false
    | Null -> true
    | Any -> false
    | Lits _ -> false
    | Concat (m, _, _, _) -> m
    | Union (m, _, _, _) -> m
    | Repeat (m, _, _, _) -> m
    | Star _ -> true
    | Comp (m, _) -> m

  let is_nullable' n =
    let rec go = function
      | Nothing -> false
      | Null -> true
      | Any -> false
      | Lits ls -> n ls
      | Concat (_, _, x, y) -> Is_nullable.concat (go x) (go y)
      | Union (_, _, x, y) -> Is_nullable.union (go x) (go y)
      | Repeat (_, _, x, i) -> Is_nullable.repeat (go x) i
      | Star _ -> true
      | Comp (_, x) -> Is_nullable.comp (go x)
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
    | Comp (_, _) -> false

  let is_infinite =
    let rec go = function
      | Nothing -> false
      | Null -> false
      | Any -> false
      | Lits _ -> false
      | Concat (_, _, x, y) -> go x || go y
      | Union (_, _, x, y) -> go x || go y
      | Repeat (_, _, x, _) -> go x
      | Star _ -> true
      | Comp (_, _) -> true (* not (go x)*)
    in go

  let rec reverse = function
    | Concat (m, h, x, y) -> Concat (m, h, reverse y, reverse x)
    | Union (m, h, x, y) -> Union (m, h, reverse x, reverse y)
    | Repeat (m, h, x, i) -> Repeat (m, h, reverse x, i)
    | Star x -> Star (reverse x)
    | Comp (m, x) -> Comp (m, reverse x)
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
    Concat (Is_nullable.concat (is_nullable x) (is_nullable y), Is_nothing.concat (is_nothing x) (is_nothing y), x, y)

  let union x y =
    Union (Is_nullable.union (is_nullable x) (is_nullable y), Is_nothing.union (is_nothing x) (is_nothing y), x, y)

  let repeat x i =
    Repeat (Is_nullable.repeat (is_nullable x) i, Is_nothing.repeat (is_nothing x) i, x, i)

  let star x =
    Star x

  let comp x =
    Comp (Is_nullable.comp (is_nullable x), x)

  let comp_nothing =
    Comp (true, Nothing)

  let comp_null =
    Comp (false, Null)

  let comp_any =
    Comp (true, Any)

  module To_seq = struct
    let nothing =
      Seq.empty

    let null =
      Seq.return Seq.empty

    let lits ls =
      Seq.map Seq.return ls

    let union cmp r1 r2 =
      let c = Seq.compare_heads cmp r1 r2 in
      Seq.fair_flatten cmp @@
      if c <= 0
      then Seq.cons r1 (Seq.cons r2 Seq.empty)
      else Seq.cons r2 (Seq.cons r1 Seq.empty)

    let concat cmp r1 r2 =
      Seq.fair_flatten cmp @@
      Seq.map (fun x ->
          Seq.map (fun y -> Seq.append x y) r2)
        r1

    let repeat cmp r i =
      Seq.fair_flatten cmp @@
      Seq.take i @@ Seq.repeat r

    let star cmp r =
      Seq.fair_flatten cmp @@
      Seq.unfold (fun acc -> Some (acc, concat cmp r acc)) null

    let comp cmp any r =
      Seq.diff cmp r (star cmp (lits any))
  end

  exception Undefined

  let to_seq ~cmp ?any (f: 'a -> 'b Seq.t) =
    let try_any () =
      match any with
      | Some any -> any
      | None -> raise Undefined
    in
    let cmp = Seq.shortlex cmp in
    let rec go x =
      Seq.deduplicate cmp @@
      match x with
      | Nothing -> To_seq.nothing
      | Null -> To_seq.null
      | Any -> To_seq.lits (try_any ())
      | Lits ls -> To_seq.lits (f ls)
      | Concat (_, _, x, y) -> To_seq.concat cmp (go x) (go y)
      | Union (_, _, x, y) -> To_seq.union cmp (go x) (go y)
      | Repeat (_, _, x, i) -> To_seq.repeat cmp (go x) i
      | Star x -> To_seq.star cmp (go x)
      | Comp (_, x) ->  To_seq.comp cmp (try_any ()) (go x)
    in go

  let dehance supply gen f =
    let (let*) (x, xp) f =
      let (y, yp) = f x in
      (y, xp @ yp)
    in
    let supply = ref supply in
    let return x = (x, []) in
    let rec go = function
      | Nothing -> return Nothing
      | Null ->  return Null
      | Any -> return Any
      | Lits ls -> return (Lits ls)
      | Concat (m, h, x, y) ->
        let* x = go x in
        let* y = go y in
        return (Concat (m, h, x, y))
      | Union (m, h, x, y) ->
        let* x = go x in
        let* y = go y in
        return (Union (m, h, x, y))
      | Repeat (m, h, x, i) ->
        let* x = go x in
        return (Repeat (m, h, x, i))
      | Star x ->
        let var, supply' = gen !supply in
        supply := supply';
        let* x = go x in
        lits (f var), [(var, union (concat (lits (f var)) x) null)]
      | Comp (m, x) ->
        let* x = go x in
        return (Comp (m, x))
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
    comp (star (comp x))

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
  let (~-) x = comp x
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
      | Comp (_, x) -> go x
    in go

  let first = 
    first' is_nullable

  let simplify' =
    let rec go = function
      | Union (m, h, x, y) ->
        (match go x, go y with
         | Nothing, x -> x
         | x, Nothing -> x
         | Comp (_, Nothing), _ -> comp_nothing
         | _, Comp (_, Nothing) -> comp_nothing
         | x, y -> Union (m, h, x, y))

      | Concat (m, h, x, y) ->
        (match go x, go y with
         | Nothing, _ -> nothing
         | _, Nothing -> nothing
         | Null, x -> x
         | x, Null -> x
         | Comp (_, Nothing), Comp (_, Nothing) -> comp_nothing
         | Comp (_, Null), Comp (_, Null) -> comp_null
         | x, y -> Concat (m, h, x, y))

      | Repeat (_, _, _, 0) -> null
      | Repeat (m, h, x, i) ->
        (match go x with
         | Nothing -> nothing
         | Null -> nothing
         | Comp (_, Nothing) -> comp_nothing
         | Comp (_, Null) -> comp_null
         | x -> Repeat (m, h, x, i))

      | Star x ->
        (match go x with
         | Null -> null
         | Nothing -> null
         | Comp (_, Nothing) -> comp_nothing
         | Comp (_, Null) -> comp_nothing
         | Star x -> x
         | x -> Star x)

      | Comp (m, x) ->
        (match go x with
         | Comp (_, x) -> x
         | x -> Comp (m, x))

      | x -> x
    in go

  let rec collect =
    let rec go = function
      | Union (_, _, x, y) ->
        Seq.deduplicate compare (Seq.sorted_merge compare (go x) (go y))
      | Concat (m, h, x, y) ->
        Seq.return (Concat (m, h, normalize x, normalize y))
      | Repeat (m, h, x, i) ->
        Seq.return (Repeat (m, h, normalize x, i))
      | Star x ->
        Seq.return (Star (normalize x))
      | Comp (m, x) ->
        Seq.return (Comp (m, normalize x))
      | x -> Seq.return x
    in go
  and normalize x =
    collect x
    |> Seq.fold_left union nothing

  let simplify x =
    simplify' @@ normalize x

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
    | Comp (_, x) ->
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
