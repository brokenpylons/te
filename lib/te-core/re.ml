(* Core set of regular expressions *)

module type OP = sig
  type +_ t

  val nothing: _ t
  val null: _ t
  val any: _ t
  val comp_nothing: _ t
  val comp_null: _ t
  val comp_any: _ t
  val lits: 'a -> 'a t
  val concat: 'a t -> 'a t -> 'a t
  val union: 'a t -> 'a t -> 'a t
  val repeat: 'a t -> int -> 'a t
  val star: 'a t -> 'a t
  val comp: 'a t -> 'a t
end

module Abstract = struct
  type 'a t =
    | Nothing
    | Null
    | Any
    | Lits of 'a
    | Concat of bool * 'a t * 'a t
    | Union of bool * 'a t * 'a t
    | Repeat of bool * 'a t * int
    | Star of 'a t
    | Comp of bool * 'a t
  [@@deriving eq, ord]

  let pp pp_lits =
    let rec go ppf = function
      | Nothing -> Fmt.string ppf "∅"
      | Null -> Fmt.string ppf "ε"
      | Any -> Fmt.string ppf "."
      | Lits x -> pp_lits ppf x
      | Concat (_, x, y) -> Fmt.pf ppf "@[@[%a@]@ @[%a@]@]" go x go y
      | Union (_, x, y) -> Fmt.pf ppf "@[(@[%a@]@,|@,@[%a@])@]" go x go y   
      | Repeat (_, x, i) -> Fmt.pf ppf "@[%a@,@[{%i}@]@]" go x i
      | Star x -> Fmt.pf ppf "@[(@[%a@]){*}@]" go x
      | Comp (_, x) -> Fmt.pf ppf "@[¬(@[%a@])@]" go x
    in go

  let is_nullable = function
    | Nothing -> false
    | Null -> true
    | Any -> false
    | Lits _ -> false
    | Concat (m, _, _) -> m
    | Union (m, _, _) -> m
    | Repeat (m, _, _) -> m
    | Star _ -> true
    | Comp (m, _) -> m

  let rec reverse = function
    | Concat (m, x, y) -> Concat (m, reverse y, reverse x)
    | Union (m, x, y) -> Union (m, reverse x, reverse y)
    | Repeat (m, x, i) -> Repeat (m, reverse x, i)
    | Star x -> Star (reverse x)
    | Comp (m, x) -> Comp (m, x)
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
    Concat (is_nullable x && is_nullable y, x, y)

  let union x y =
    Union (is_nullable x || is_nullable y, x, y)

  let repeat x = function
    | 0 -> Repeat (true, x, 0)
    | i when i > 0 -> Repeat (is_nullable x, x, i)
    | _ -> assert false

  let star x =
    Star x

  let comp x =
    Comp (not @@ is_nullable x, x)

  let comp_nothing =
    Comp (true, Nothing)

  let comp_null =
    Comp (false, Null)

  let comp_any =
    Comp (true, Any)
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

  val is_nullable: t -> bool
  val derivative: lits -> t -> t
  val derivative': lits list -> t -> t

  val first: t -> lits Seq.t
  val simplify: t -> t
end

module Concrete(Lits: LITS): CONCRETE with type lits = Lits.t = struct
  include Abstract
  type lits = Lits.t
  type t = Lits.t Abstract.t
  [@@deriving eq, ord, show]

  let first = 
    let rec go = function
      | Nothing -> Seq.empty
      | Null -> Seq.empty
      | Any -> Seq.empty
      | Lits p -> Seq.return p
      | Concat (_, x, y) when is_nullable x -> Seq.append (go x) (go y)
      | Concat (_, x, _) -> go x
      | Union (_, x, y) -> Seq.append (go x) (go y)
      | Repeat (_, x, _) -> go x
      | Star x -> go x
      | Comp (_, x) -> go x
    in go

  let rec simplify = function
    | Union (m, x, y) ->
      (match simplify x, simplify y with
       | Union (n, x, y), z -> Union (m, x, Union (n, y, z))
       | Nothing, x -> x
       | x, Nothing -> x
       | Comp (_, Nothing), _ -> comp_nothing
       | _, Comp (_, Nothing) -> comp_nothing
       | x, y when equal x y -> x
       | x, y when compare x y > 0 -> Union (m, y, x)
       | x, y -> Union (m, x, y))

    | Concat (m, x, y) -> 
      (match simplify x, simplify y with
       | Concat (n, x, y), z -> Concat (m, x, Concat (n, y, z))
       | Nothing, _ -> nothing
       | _, Nothing -> nothing
       | Null, x -> x
       | x, Null -> x
       | Comp (_, Nothing), Comp (_, Nothing) -> comp_nothing
       | Comp (_, Null), Comp (_, Null) -> comp_null
       | x, y -> Concat (m, x, y))

    | Repeat (_, _, 0) -> null
    | Repeat (m, x, i) -> 
      (match simplify x with
       | Nothing -> nothing
       | Null -> nothing
       | Comp (_, Nothing) -> comp_nothing
       | Comp (_, Null) -> comp_null
       | x -> Repeat (m, x, i))

    | Star x -> 
      (match simplify x with
       | Null -> null
       | Nothing -> null
       | Comp (_, Nothing) -> comp_nothing
       | Comp (_, Null) -> comp_nothing
       | Star x -> x
       | x -> Star x)

    | Comp (m, x) -> 
      (match simplify x with
       | Comp (_, x) -> x
       | x -> Comp (m, x))

    | x -> x

  let rec derivative s = function
    | Nothing -> nothing
    | Null -> nothing
    | Any -> null
    | Lits p -> 
      if Lits.subset s p
      then null
      else nothing
    | Concat (_, x, y) when is_nullable x -> union (concat (derivative s x) y) (derivative s y)
    | Concat (_, x, y) -> concat (derivative s x) y
    | Union (_, x, y) -> union (derivative s x) (derivative s y)
    | Repeat (_, x, i) -> 
      (match i with
       | 0 -> nothing
       | i -> concat (derivative s x) (repeat x (pred i)))
    | Star x ->
      concat (derivative s x) (star x)
    | Comp (_, x) ->
      comp (derivative s x)

  let derivative' ss x =
    List.fold_left (Fun.flip derivative) x ss
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
