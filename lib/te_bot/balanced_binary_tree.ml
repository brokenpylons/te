open! Prelude

module type ELT = sig
  type t
  val compare: t -> t -> int
end

module Set = struct
  module type TREE = sig
    type tag
    val pp_tag: tag Fmt.t
    type 'a t = Empty | Node of tag * 'a * 'a t * 'a t

    val height: 'a t -> int
    val cardinal: 'a t -> int
    val singleton: 'a -> 'a t
    val join: 'a -> 'a t -> 'a t -> 'a t

    val balanced: 'a t -> bool
    val balance_left: 'a -> 'a t -> 'a t -> 'a t
    val balance_right: 'a -> 'a t -> 'a t -> 'a t
  end

  module type S = sig
    type elt
    type t

    include Set.CORE with type elt := elt and type t := t
    include Set.BINARY with type elt := elt and type t := t
    include Set.SEQUENTIAL with type elt := elt and type t := t
    val pp: elt Fmt.t -> t Fmt.t
    val add_with: (elt -> elt -> elt) -> elt -> t -> t

    val choose: t -> elt option
    val balanced: t -> bool
    val cardinal: t -> int
    val disjoint: t -> t -> bool
    val subset: t -> t -> bool

    val find: elt -> t -> elt
    val filter: (elt -> bool) -> t -> t
    val iter: (elt -> unit) -> t -> unit
    val for_all: (elt -> bool) -> t -> bool
    val exists: (elt -> bool) -> t -> bool
    val map: (elt -> elt) -> t -> t
    (*val flat_map: (elt -> t) -> t -> t*)

    val (<|>): t -> t -> t
  end

  module Height_tree: TREE = struct
    type tag = int
    type 'a t = Empty | Node of tag * 'a * 'a t * 'a t

    let pp_tag =
      Fmt.int

    let height = function
      | Empty -> 0
      | Node (h, _, _, _) -> h
    [@@inline]

    let rec cardinal = function
      | Empty -> 0
      | Node (_, _, l, r) -> 1 + cardinal l + cardinal r

    let singleton x =
      Node (1, x, Empty, Empty)
    [@@inline]

    let node x l r =
      let hl = height l
      and hr = height r in
      Node ((if hl >= hr then hl + 1 else hr + 1), x, l, r)
    [@@inline]

    let height = function
      | Empty -> 0
      | Node (h, _, _, _) -> h
    [@@inline]

    let rec balanced = function
      | Empty -> true
      | Node (_, _, l, r) ->
        abs (height r - height l) <= 2 && balanced l && balanced r

    let balance_left x l r =
      if height l > height r + 1 then 
        match l with
        | Node (_, y, ll, lr) ->
          if height ll >= height lr
          then node y ll (node x lr r)
          else (match lr with
              | Node (_, z, lrl, lrr) -> node z (node y ll lrl) (node x lrr r)
              | Empty -> assert false)
        | Empty -> assert false
      else 
        node x l r

    let balance_right x l r =
      if height l + 1 < height r then 
        match r with
        | Node (_, y, rl, rr) ->
          if height rl <= height rr
          then node y (node x l rl) rr
          else (match rl with
              | Node (_, z, rll, rlr) -> node z (node x l rll) (node y rlr rr)
              | Empty -> assert false)
        | Empty -> assert false
      else
        node x l r

    let rec add_max x = function
      | Empty -> singleton x
      | Node (_, y, l, r) -> balance_right y l (add_max x r)

    let rec add_min x = function
      | Empty -> singleton x
      | Node (_, y, l, r) -> balance_left y (add_min x l) r

    let rec join x l r =
      match l, r with
      | Empty, Empty -> singleton x
      | Empty, r -> add_min x r
      | l, Empty -> add_max x l
      | Node (lh, y, ll, lr), Node (rh, z, rl, rr) ->
        if lh > rh + 1 then
          balance_left y ll (join x lr r)
        else if lh + 1 < rh then
          balance_right z (join x l rl) rr
        else
          node x l r
  end

  module Size_tree: TREE = struct
    type tag = int
    type 'a t = Empty | Node of tag * 'a * 'a t * 'a t
    let ratio = 5

    let pp_tag =
      Fmt.int

    let rec height = function
      | Empty -> 0
      | Node (_, _, l, r) -> 
        let hl = height l
        and hr = height r in
        if hl >= hr then hl + 1 else hr + 1

    let size = function
      | Empty -> 1
      | Node (s, _, _, _) -> s
    [@@inline]

    let cardinal = function
      | Empty -> 0
      | Node (s, _, _, _) -> (s + 1) / 2

    let singleton x =
      Node (3, x, Empty, Empty)
    [@@inline]

    let node x l r =
      let ls = size l
      and rs = size r in
      Node (1 + ls + rs, x, l, r)
    [@@inline]

    let rec balanced = function
      | Empty -> true
      | Node (_, _, l, r) ->
        size l <= ratio * size r && ratio * size l >= size r && balanced l && balanced r

    let balance_left x l r =
      if size l > ratio * size r then 
        match l with
        | Node (_, y, ll, lr) ->
          if size ll > size lr
          then node y ll (node x lr r)
          else (match lr with
              | Node (_, z, lrl, lrr) -> node z (node y ll lrl) (node x lrr r)
              | Empty -> assert false)
        | Empty -> assert false
      else 
        node x l r

    let balance_right x l r =
      if ratio * size l < size r then 
        match r with
        | Node (_, y, rl, rr) ->
          if size rl < size rr
          then node y (node x l rl) rr
          else (match rl with
              | Node (_, z, rll, rlr) -> node z (node x l rll) (node y rlr rr)
              | Empty -> assert false)
        | Empty -> assert false
      else
        node x l r

    let rec add_max x = function
      | Empty -> singleton x
      | Node (_, y, l, r) -> balance_right y l (add_max x r)

    let rec add_min x = function
      | Empty -> singleton x
      | Node (_, y, l, r) -> balance_left y (add_min x l) r

    let rec join x l r =
      match l, r with
      | Empty, Empty -> singleton x
      | Empty, r -> add_min x r
      | l, Empty -> add_max x l
      | Node (ls, y, ll, lr), Node (rs, z, rl, rr) ->
        if ls > ratio * rs then
          balance_left y ll (join x lr r)
        else if ratio * ls < rs then
          balance_right z (join x l rl) rr
        else
          node x l r
  end

  module Make(Elt: ELT)(Tree: TREE): S with type elt = Elt.t = struct
    include Tree

    type elt = Elt.t
    type t = elt Tree.t
    type enumerator = Cons of Elt.t * t * enumerator | Nil


    let empty = Empty

    let is_empty = function
      | Empty -> true
      | _ -> false
    [@@inline]

    let the = function
      | Empty -> assert false
      | Node (_, x, Empty, Empty) ->
        x
      | _ -> assert false

    let choose = function
      | Empty -> None
      | Node (_, x, _, _) ->
        Some x

    let balanced = Tree.balanced

    let cardinal = Tree.cardinal

    let rec cons t e =
      match t with
      | Empty -> e
      | Node (_, x, l, r) -> cons l (Cons (x, r, e))

    let rec compare' e1 e2 =
      match e1, e2 with
      | Nil, Nil -> 0
      | Nil, _ -> -1
      | _, Nil -> 1
      | Cons (x1, t1, e1'), Cons (x2, t2, e2') ->
        let c = Elt.compare x1 x2 in
        if c <> 0
        then c
        else compare' (cons t1 e1') (cons t2 e2')

    let compare t1 t2 = compare' (cons t1 Nil) (cons t2 Nil)

    let equal t1 t2 = compare t1 t2 = 0

    let rec fold f acc = function
      | Empty -> acc 
      | Node (_, y, l, r) ->
        fold f (f y (fold f acc l)) r

    let rec for_all f = function
      | Empty -> true
      | Node (_, x, l, r) -> f x && for_all f l && for_all f r

    let rec exists f = function
      | Empty -> false
      | Node (_, x, l, r) -> f x || exists f l || exists f r

    let rec add x = function
      | Empty -> singleton x
      | Node (tag, y, l, r) as t ->
        let c = Elt.compare x y in
        if c = 0 then 
          if x == y
          then t
          else Node (tag, x, l, r)
        else if c < 0 then
          let l' = add x l in
          if l == l' then t else balance_left y l' r
        else 
          let r' = add x r in 
          if r == r' then t else balance_right y l r'

    include Set.Sequential(struct
        type nonrec t = t
        type nonrec elt = elt
        let add = add
        let empty = empty
        let fold = fold
      end)

    let rec add_with f x = function
      | Empty -> singleton x
      | Node (tag, y, l, r) as t ->
        let c = Elt.compare x y in
        if c = 0 then 
          if x == y
          then t
          else Node (tag, f x y, l, r)
        else if c < 0 then
          let l' = add_with f x l in
          if l == l' then t else balance_left y l' r
        else 
          let r' = add_with f x r in 
          if r == r' then t else balance_right y l r'

    let rec split x = function
      | Empty -> (false, Empty, Empty)
      | Node (_, y, l, r) ->
        let c = Elt.compare x y in
        if c = 0 then 
          (true, l, r) 
        else if c < 0 then
          let (b, ll, lr) = split x l in
          (b, ll, join y lr r)
        else 
          let (b, rl, rr) = split x r in
          (b, join y l rl, rr)

    let rec split_max = function
      | Empty -> assert false
      | Node (_, x, l, r) ->
        if is_empty r
        then (x, l)
        else 
          let (x', l') = split_max r in
          (x', join x l l')

    let concat l r =
      if is_empty l
      then r
      else
        let (x, l') = split_max l in
        join x l' r

    let rec union t1 t2 =
      match t1, t2 with
      | Empty, Empty -> Empty
      | Empty, _ -> t2
      | _, Empty -> t1
      | t1, Node (_, x, l2, r2) ->
        let (_, l1, r1) = split x t1 in
        let l = union l1 l2 
        and r = union r1 r2 in
        join x l r

    let (<|>) = union

    let rec disjoint t1 t2 =
      match t1, t2 with
      | Empty, _
      | _, Empty -> true
      | t1, Node (_, x, l2, r2) ->
        let (b, l1, r1) = split x t1 in
        if b
        then false
        else disjoint l1 l2 && disjoint r1 r2 

    let rec inter t1 t2 =
      match t1, t2 with
      | Empty, Empty -> Empty
      | Empty, _ -> Empty
      | _, Empty -> Empty
      | t1, Node (_, x, l2, r2) ->
        let (b, l1, r1) = split x t1 in
        let l = inter l1 l2 
        and r = inter r1 r2 in
        if b
        then join x l r
        else concat l r

    let rec diff t1 t2 =
      match t1, t2 with
      | Empty, Empty -> Empty
      | Empty, _ -> Empty
      | _, Empty -> t1
      | t1, Node (_, x, l2, r2) ->
        let (_, l1, r1) = split x t1 in
        let l = diff l1 l2 
        and r = diff r1 r2 in
        concat l r

    let rec subset' e1 e2 =
      match e1, e2 with
      | Nil, _ -> true
      | _, Nil -> false
      | Cons (x1, t1, e1'), Cons (x2, t2, e2') ->
        let c = Elt.compare x1 x2 in
        if c == 0 then 
          subset' (cons t1 e1') (cons t2 e2')
        else if c > 0 then
          subset' e1 (cons t2 e2')
        else false

    let subset t1 t2 = subset' (cons t1 Nil) (cons t2 Nil)

    let rec remove x = function
      | Empty -> Empty
      | Node (_, y, l, r) as t -> 
        let c = Elt.compare x y in
        if c = 0 then 
          concat l r
        else if c < 0 then
          let l' = remove x l in
          if l == l' then t else balance_left y l' r
        else 
          let r' = remove x r in 
          if r == r' then t else balance_right y l r'

    let rec to_seq' t tl =
      match t with
      | Empty -> tl
      | Node (_, x, l, r) ->
        to_seq' l (fun () -> Seq.Cons (x, to_seq' r tl))

    let to_seq t = to_seq' t Seq.empty

    let rec to_list' t tl =
      match t with
      | Empty -> tl
      | Node (_, x, l, r) ->
        to_list' l (x :: to_list' r tl)

    let to_list t = to_list' t []

    let rec find x = function
      | Empty -> raise Not_found
      | Node (_, y, l, r) ->
        let c = Elt.compare x y in
        if c = 0
        then y
        else if c < 0
        then find x l
        else find x r

    let rec mem x = function
      | Empty -> false
      | Node (_, y, l, r) ->
        let c = Elt.compare x y in
        c = 0 || if c < 0 then mem x l else mem x r

    let rec filter f = function
      | Empty -> Empty
      | Node (_, x, l, r) as t ->
        let l' = filter f l in
        let s = f x in
        let r' = filter f r in
        if s
        then if l == l' && r == r' then t else join x l' r'
        else concat l' r'

    let rec iter f = function
      | Empty -> ()
      | Node (_, x, l, r) ->
        iter f l; f x; iter f r

    let map f t =
      fold (add % f) empty t

    let rec pp' sep pp_elt ppf = function
      | Nil -> Fmt.nop ppf ""
      | Cons (x, t, e) ->
        Fmt.pf ppf (sep ^^ "%a%a") pp_elt x (pp' "@ " pp_elt) (cons t e)

    let pp pp_elt ppf t =
      Fmt.pf ppf "@[%a@]" (pp' "" pp_elt) (cons t Nil)
  end

  module Height(Elt: ELT) = Make(Elt)(Height_tree)
  module Size(Elt: ELT) = Make(Elt)(Size_tree)
end

module Map = struct
  module type TREE = sig
    type tag
    val pp_tag: tag Fmt.t
    type ('a, 'b) t = Empty | Node of tag * 'a * 'b * ('a, 'b) t * ('a, 'b) t

    val height: ('a, 'b) t -> int
    val cardinal: ('a, 'b) t -> int
    val singleton: 'a -> 'b -> ('a, 'b) t
    val join: 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

    val balanced: ('a, 'b) t -> bool
    val balance_left: 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
    val balance_right: 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  end

  module type S = sig
    type elt
    type 'a t

    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val pp: elt Fmt.t -> 'a Fmt.t -> 'a t Fmt.t

    val is_empty: 'a t -> bool
    val empty: 'a t
    val singleton: elt -> 'a -> 'a t
    val add: elt -> 'a -> 'a t -> 'a t
    val remove: elt -> 'a t -> 'a t

    val balanced: 'a t -> bool
    val cardinal: 'a t -> int

    val find: elt -> 'a t -> 'a
    val find_opt: elt -> 'a t -> 'a option
    val mem: elt -> 'a t -> bool
    val to_seq: 'a t -> (elt * 'a) Seq.t
    val of_seq: (elt * 'a) Seq.t -> 'a t

    val add_with: (elt -> elt -> elt) -> ('a -> 'a -> 'a) -> elt -> 'a -> 'a t -> 'a t

    val merge: (elt -> 'a option -> 'a option -> 'a option) -> 'a t -> 'a t -> 'a t
    val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

    val the: 'a t -> elt * 'a
    val fold: (elt -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
    val map: (elt -> 'a -> 'b) -> 'a t -> 'b t
    val for_all: (elt -> 'a -> bool) -> 'a t -> bool
    val exists: (elt -> 'a -> bool) -> 'a t -> bool
    val modify: elt -> ('a -> 'a) -> 'a t -> 'a t
    val update: elt -> ('a option -> 'a option) -> 'a t -> 'a t
    val filter: (elt -> 'a -> bool) -> 'a t -> 'a t
    val (&): (elt * 'a) -> 'a t -> 'a t
    val to_fun: 'a t -> (elt -> 'a)

    val domain_disjoint: 'a t -> 'a t -> bool
  end

  module Height_tree: TREE = struct
    type tag = int
    type ('a, 'b) t = Empty | Node of tag * 'a * 'b * ('a, 'b) t * ('a, 'b) t

    let pp_tag =
      Fmt.int

    let height = function
      | Empty -> 0
      | Node (h, _, _, _, _) -> h
    [@@inline]

    let rec cardinal = function
      | Empty -> 0
      | Node (_, _, _, l, r) -> 1 + cardinal l + cardinal r

    let singleton x xp =
      Node (1, x, xp, Empty, Empty)
    [@@inline]

    let node x xp l r =
      let hl = height l
      and hr = height r in
      Node ((if hl >= hr then hl + 1 else hr + 1), x, xp, l, r)
    [@@inline]

    let height = function
      | Empty -> 0
      | Node (h, _, _, _, _) -> h
    [@@inline]

    let rec balanced = function
      | Empty -> true
      | Node (_, _, _, l, r) ->
        abs (height r - height l) <= 2 && balanced l && balanced r

    let balance_left x xp l r =
      if height l > height r + 1 then 
        match l with
        | Node (_, y, yp, ll, lr) ->
          if height ll >= height lr
          then node y yp ll (node x xp lr r)
          else (match lr with
              | Node (_, z, zp, lrl, lrr) -> node z zp (node y yp ll lrl) (node x xp lrr r)
              | Empty -> assert false)
        | Empty -> assert false
      else 
        node x xp l r

    let balance_right x xp l r =
      if height l + 1 < height r then 
        match r with
        | Node (_, y, yp, rl, rr) ->
          if height rl <= height rr
          then node y yp (node x xp l rl) rr
          else (match rl with
              | Node (_, z, zp, rll, rlr) -> node z zp (node x xp l rll) (node y yp rlr rr)
              | Empty -> assert false)
        | Empty -> assert false
      else
        node x xp l r

    let rec add_max x xp = function
      | Empty -> singleton x xp
      | Node (_, y, yp, l, r) -> balance_right y yp l (add_max x xp r)

    let rec add_min x xp = function
      | Empty -> singleton x xp
      | Node (_, y, yp, l, r) -> balance_left y yp (add_min x xp l) r

    let rec join x xp l r =
      match l, r with
      | Empty, Empty -> singleton x xp
      | Empty, r -> add_min x xp r
      | l, Empty -> add_max x xp l
      | Node (lh, y, yp, ll, lr), Node (rh, z, zp, rl, rr) ->
        if lh > rh + 1 then
          balance_left y yp ll (join x xp lr r)
        else if lh + 1 < rh then
          balance_right z zp (join x xp l rl) rr
        else
          node x xp l r
  end

  module Size_tree: TREE = struct
    type tag = int
    type ('a, 'b) t = Empty | Node of tag * 'a * 'b * ('a, 'b) t * ('a, 'b) t
    let ratio = 5

    let pp_tag =
      Fmt.int

    let rec height = function
      | Empty -> 0
      | Node (_, _, _, l, r) -> 
        let hl = height l
        and hr = height r in
        if hl >= hr then hl + 1 else hr + 1

    let size = function
      | Empty -> 1
      | Node (s, _, _, _, _) -> s
    [@@inline]

    let cardinal = function
      | Empty -> 0
      | Node (s, _, _ , _, _) -> (s + 1) / 2

    let singleton x xp =
      Node (3, x, xp, Empty, Empty)
    [@@inline]

    let node x xp l r =
      let ls = size l
      and rs = size r in
      Node (1 + ls + rs, x, xp, l, r)
    [@@inline]

    let rec balanced = function
      | Empty -> true
      | Node (_, _, _, l, r) ->
        size l <= ratio * size r && ratio * size l >= size r && balanced l && balanced r

    let balance_left x xp l r =
      if size l > ratio * size r then 
        match l with
        | Node (_, y, yp, ll, lr) ->
          if size ll > size lr
          then node y yp ll (node x xp lr r)
          else (match lr with
              | Node (_, z, zp, lrl, lrr) -> node z zp (node y yp ll lrl) (node x xp lrr r)
              | Empty -> assert false)
        | Empty -> assert false
      else 
        node x xp l r

    let balance_right x xp l r =
      if ratio * size l < size r then 
        match r with
        | Node (_, y, yp, rl, rr) ->
          if size rl < size rr
          then node y yp (node x xp l rl) rr
          else (match rl with
              | Node (_, z, zp, rll, rlr) -> node z zp (node x xp l rll) (node y yp rlr rr)
              | Empty -> assert false)
        | Empty -> assert false
      else
        node x xp l r

    let rec add_max x xp = function
      | Empty -> singleton x xp
      | Node (_, y, yp, l, r) -> balance_right y yp l (add_max x xp r)

    let rec add_min x xp = function
      | Empty -> singleton x xp
      | Node (_, y, yp, l, r) -> balance_left y yp (add_min x xp l) r

    let rec join x xp l r =
      match l, r with
      | Empty, Empty -> singleton x xp
      | Empty, r -> add_min x xp r
      | l, Empty -> add_max x xp l
      | Node (ls, y, yp, ll, lr), Node (rs, z, zp, rl, rr) ->
        if ls > ratio * rs then
          balance_left y yp ll (join x xp lr r)
        else if ratio * ls < rs then
          balance_right z zp (join x xp l rl) rr
        else
          node x xp l r
  end

  module Make(Elt: ELT)(Tree: TREE): S with type elt = Elt.t = struct
    include Tree

    type elt = Elt.t
    type 'a t = (elt, 'a) Tree.t
    type 'a enumerator = Cons of elt * 'a * 'a t * 'a enumerator | Nil

    let empty = Empty

    let is_empty = function
      | Empty -> true
      | _ -> false
    [@@inline]

    let the = function
      | Empty -> assert false
      | Node (_, x, xp, Empty, Empty) ->
        (x, xp)
      | _ -> assert false

    let balanced = Tree.balanced

    let cardinal = Tree.cardinal

    let rec cons t e =
      match t with
      | Empty -> e
      | Node (_, x, xp, l, r) -> cons l (Cons (x, xp, r, e))

    let rec compare' cmp e1 e2 =
      match e1, e2 with
      | Nil, Nil -> 0
      | Nil, _ -> -1
      | _, Nil -> 1
      | Cons (x1, xp1, t1, e1'), Cons (x2, xp2, t2, e2') ->
        let c = Elt.compare x1 x2 in
        if c <> 0 then c else 
        let c = cmp xp1 xp2 in
        if c <> 0 then c else 
        compare' cmp (cons t1 e1') (cons t2 e2')

    let compare cmp t1 t2 = compare' cmp (cons t1 Nil) (cons t2 Nil)

    let rec equal' eq e1 e2 =
      match e1, e2 with
      | Nil, Nil -> true
      | Nil, _ | _, Nil -> false
      | Cons (x1, xp1, t1, e1'), Cons (x2, xp2, t2, e2') ->
        Elt.compare x1 x2 = 0 && eq xp1 xp2 && equal' eq (cons t1 e1') (cons t2 e2')

    let equal eq t1 t2 = equal' eq (cons t1 Nil) (cons t2 Nil)

    let rec fold f acc = function
      | Empty -> acc 
      | Node (_, x, xp, l, r) ->
        fold f (f x xp (fold f acc l)) r

    let rec map f = function
      | Empty -> Empty
      | Node (tag, x, xp, l, r) ->
        Node (tag, x, f x xp, map f l, map f r)

    let rec for_all f = function
      | Empty -> true
      | Node (_, x, xp, l, r) -> f x xp && for_all f l && for_all f r

    let rec exists f = function
      | Empty -> false
      | Node (_, x, xp, l, r) -> f x xp || exists f l || exists f r

    let rec add x xp = function
      | Empty -> singleton x xp
      | Node (tag, y, yp, l, r) as t ->
        let c = Elt.compare x y in
        if c = 0 then 
          if xp == yp
          then t
          else Node (tag, x, xp, l, r)
        else if c < 0 then
          let l' = add x xp l in
          if l == l' then t else balance_left y yp l' r
        else 
          let r' = add x xp r in 
          if r == r' then t else balance_right y yp l r'

    let rec add_with f g x xp = function
      | Empty -> singleton x xp
      | Node (tag, y, yp, l, r) as t ->
        let c = Elt.compare x y in
        if c = 0 then 
          if x == y && xp == yp
          then t
          else Node (tag, f x y, g xp yp, l, r)
        else if c < 0 then
          let l' = add_with f g x xp l in
          if l == l' then t else balance_left y yp l' r
        else 
          let r' = add_with f g x xp r in 
          if r == r' then t else balance_right y yp l r'

    let rec split x = function
      | Empty -> (None, Empty, Empty)
      | Node (_, y, yp, l, r) ->
        let c = Elt.compare x y in
        if c = 0 then 
          (Some yp, l, r) 
        else if c < 0 then
          let (b, ll, lr) = split x l in
          (b, ll, join y yp lr r)
        else 
          let (b, rl, rr) = split x r in
          (b, join y yp l rl, rr)

    let rec split_max = function
      | Empty -> assert false
      | Node (_, x, xp, l, r) ->
        if is_empty r
        then (x, xp, l)
        else 
          let (x', xp', l') = split_max r in
          (x', xp', join x xp l l')

    let concat l r =
      if is_empty l
      then r
      else
        let (x, xp, l') = split_max l in
        join x xp l' r

    let rec modify x f = function
      | Empty -> raise Not_found
      | Node (tag, y, yp, l, r) as t ->
        let c = Elt.compare x y in
        if c = 0 then 
          let xp = f yp in
          if xp == yp
          then t
          else Node (tag, x, xp, l, r)
        else if c < 0 then
          let l' = modify x f l in
          if l == l' then t else balance_left y yp l' r
        else 
          let r' = modify x f r in 
          if r == r' then t else balance_right y yp l r'

    let rec update x f = function
      | Empty ->
        begin
          match f None with
          | None -> Empty
          | Some xp -> singleton x xp
        end
      | Node (tag, y, yp, l, r) as t ->
        let c = Elt.compare x y in
        if c = 0 then 
          begin
            match f (Some yp) with
            | None -> concat l r
            | Some xp ->  
              if xp == yp
              then t
              else Node (tag, x, xp, l, r)
          end
        else if c < 0 then
          let l' = update x f l in
          if l == l' then t else balance_left y yp l' r
        else 
          let r' = update x f r in 
          if r == r' then t else balance_right y yp l r'

    let rec domain_disjoint t1 t2 =
      match t1, t2 with
      | Empty, _
      | _, Empty -> true
      | t1, Node (_, x, _, l2, r2) ->
        let (s, l1, r1) = split x t1 in
        match s with
        | Some _ -> false
        | None -> domain_disjoint l1 l2 && domain_disjoint r1 r2 

    let rec merge f t1 t2 =
      match t1, t2 with
      | Empty, Empty -> Empty
      |  Node (_, x, xp, l, r), Empty ->
        (match f x (Some xp) None with
        | Some xp -> join x xp l r
        | None -> Empty)
      | t1, Node (_, x, xp, l2, r2) ->
        let (s, l1, r1) = split x t1 in
        let l = merge f l1 l2 
        and r = merge f r1 r2 in
        (match f x s (Some xp) with
        | Some xp -> join x xp l r
        | None -> concat l r)

    let rec union f t1 t2 =
      match t1, t2 with
      | Empty, Empty -> Empty
      | Empty, _ -> t2
      | _, Empty -> t1
      | t1, Node (_, x, xp, l2, r2) ->
        let (s, l1, r1) = split x t1 in
        let l = union f l1 l2 
        and r = union f r1 r2 in
        match s with
        | Some xp' -> join x (f x xp' xp) l r
        | None -> join x xp l r

    let rec filter f = function
      | Empty -> Empty
      | Node (_, x, xp, l, r) as t ->
        let l' = filter f l in
        let s = f x xp in
        let r' = filter f r in
        if s
        then if l == l' && r == r' then t else join x xp l' r'
        else concat l' r'

    let rec remove x = function
      | Empty -> Empty
      | Node (_, y, yp, l, r) as t -> 
        let c = Elt.compare x y in
        if c = 0 then 
          concat l r
        else if c < 0 then
          let l' = remove x l in
          if l == l' then t else balance_left y yp l' r
        else 
          let r' = remove x r in 
          if r == r' then t else balance_right y yp l r'

    let (&) (x, xp) t = add x xp t

    let add_seq s t =
      Seq.fold_left (fun t (x, xp) -> add x xp t) t s

    let of_seq s = 
      add_seq s empty

    let rec to_seq' t tl =
      match t with
      | Empty -> tl
      | Node (_, x, xp, l, r) ->
        to_seq' l (fun () -> Seq.Cons ((x, xp), to_seq' r tl))

    let to_seq t = to_seq' t Seq.empty

    let rec find x = function
      | Empty -> raise Not_found
      | Node (_, y, ys, l, r) ->
        let c = Elt.compare x y in
        if c = 0
        then ys
        else if c < 0
        then find x l
        else find x r

    let find_opt x t = 
      try Some (find x t) 
      with Not_found -> None

    let to_fun t x =
      find x t

    let rec mem x = function
      | Empty -> false
      | Node (_, y, _, l, r) ->
        let c = Elt.compare x y in
        c = 0 || if c < 0 then mem x l else mem x r

    let rec pp' pp_elt pp_p ppf = function
      | Nil -> Fmt.string ppf "nil"
      | Cons (x, xp, t, e) ->
        Fmt.pf ppf "@[(%a, %a), %a@]" pp_elt x pp_p xp (pp' pp_elt pp_p) (cons t e)

    let pp pp_elt pp_p ppf t = pp' pp_elt pp_p ppf (cons t Nil)
  end

  module Height(Elt: ELT) = Make(Elt)(Height_tree)
  module Size(Elt: ELT) = Make(Elt)(Size_tree)
end
