open! Prelude

module type SET0 = sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool
end

module type SET1 = sig
  include SET0

  val union: t -> t -> t
end

module type SET2 = sig
  include SET1

  val inter: t -> t -> t
  val diff: t -> t -> t
  val is_empty: t -> bool
  val empty: t
end

module type SET3 = sig
  include SET2

  type elt
  val singleton: elt -> t
  val the: t -> elt
  val add: elt -> t -> t
  val fold: (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc

  include Set.SEQUENTIAL with type t := t and type elt := elt
end

module type SET4 = sig
  include SET3

  val remove: elt -> t -> t
end

module type MAP = sig
  include Map.CORE
  val update: elt -> ('a option -> 'a option) -> 'a t -> 'a t
  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter: (elt -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val diff:  (elt -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val filter: (elt -> 'a -> bool) -> 'a t -> 'a t
end

module type S0 = sig
  type key
  type values
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool 

  val empty: t
  val is_empty: t -> bool
  val singleton_multiple: key -> values -> t

  val domain_mem: key -> t -> bool

  val the_multiple: t -> key * values
  val fold_multiple: (key -> values -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  val to_seq_multiple: t -> (key * values) Seq.t

  val find_multiple: key -> t -> values
  val find_multiple_or: default:values -> key -> t -> values

  val filter_multiple: (key -> values -> bool) -> t -> t
  val remove_multiple: key -> t -> t
end

module Make0(M: MAP)(S: SET0) = struct
  type key = M.elt
  type values = S.t
  type t = S.t M.t

  let compare = M.compare S.compare 
  let equal = M.equal S.equal

  let empty = M.empty
  let is_empty = M.is_empty
  let singleton_multiple = M.singleton

  let domain_mem = M.mem

  let the_multiple = M.the 
  let fold_multiple = M.fold
  let filter_multiple = M.filter

  let to_seq_multiple t =
    fold_multiple (fun x xp -> Seq.cons (x, xp)) Seq.empty t

  let find_multiple = M.find

  let find_multiple_or ~default x t =
    try find_multiple x t
    with Not_found -> default

  let remove_multiple = M.remove
end

module type S1 = sig
  include S0

  val add_multiple: key -> values -> t -> t
  val of_seq_multiple: (key * values) Seq.t -> t
  val union: t -> t -> t
  val (<|>): t -> t -> t
end

module Make1(M: MAP)(S: SET1) = struct
  include Make0(M)(S)

  let add_multiple x xp t =
    M.update x (function Some s -> Some (S.union xp s) | None -> Some xp) t

  let add_seq_multiple s t =
    Seq.fold_left (fun t (k, v) -> add_multiple k v t) t s

  let of_seq_multiple s =
    add_seq_multiple s empty

  let union t1 t2 =
    M.union (fun _ -> S.union) t1 t2

  let (<|>) = union
end

module type S2 = sig
  include S1

  val inter: t -> t -> t
  val diff: t -> t -> t
  val find_multiple_or_empty: key -> t -> values
end

module Make2(M: MAP)(S: SET2) = struct
  include Make1(M)(S)

  let if_empty xp =
    if S.is_empty xp then None else Some xp

  let inter t1 t2 =
    M.inter (fun _ -> if_empty %% S.inter) t1 t2

  let diff t1 t2 =
    M.diff (fun _ -> if_empty %% S.diff) t1 t2

  let find_multiple_or_empty x t =
    try find_multiple x t
    with Not_found -> S.empty
end

module type S3 = sig
  include S2
  type value
  type elt = key * value

  val singleton: key -> value -> t

  val fold: (key -> value -> 'acc -> 'acc) -> 'acc -> t -> 'acc

  val add: key -> value -> t -> t
  val the: t -> key * value

  include Set.SEQUENTIAL with type t := t and type elt := elt
  module Set: sig
    include SET3 with type t = t and type elt = elt
    val empty: t
  end
end

module Make3(M: MAP)(S: SET3) = struct
  include Make2(M)(S)
  type value = S.elt
  type elt = M.elt * S.elt

  let singleton k v = M.singleton k (S.singleton v)

  let fold f acc t =
    M.fold (fun x -> (Fun.flip @@ S.fold (fun xp -> f x xp))) acc t

  let add x xp t =
    M.update x (function Some s -> Some (S.add xp s) | None -> Some (S.singleton xp)) t

  let the t =
    let (x, xp) = M.the t in
    (x, S.the xp)

  let if_empty xp =
    if S.is_empty xp then None else Some xp

  let inter t1 t2 =
    M.inter (fun _ -> if_empty %% S.inter) t1 t2

  let diff t1 t2 =
    M.diff (fun _ -> if_empty %% S.diff) t1 t2

  module X = struct
    type nonrec t = t
    type nonrec elt = elt
    let equal = equal
    let compare = compare
    let union = union
    let add (x, xp) t = add x xp t
    let empty = empty
    let fold f = fold (fun x xp -> f (x, xp))
    let inter = inter
    let diff = diff
    let is_empty = is_empty
    let singleton (k, v) = singleton k v
    let the = the
  end
  include Set.Sequential(X)
  module Set = struct
    include X
    include Set.Sequential(X)
  end
end
