(*
   A map based relation.

   Similar to refine map, but it doesn't perserve the partitions.
   The partitions mappings are in a sense stored in a decomposed form and then reasembed.
   That also means they cannot be iterated over, without knowing what the partitions are.

   For example:
     {1; 2} -> {4; 5; 6}
     {3} -> {7}
   is stored as:
     1 -> {4; 5; 6}
     2 -> {4; 5; 6}
     3 -> {7}
   and then a search for {2; 3} is performed by retrieving {4; 5; 6} U {7} = {4; 5; 6; 7}

   However, if we have a partition of size 1, for example {1} -> {4; 5; 6} only a single lookup is needed.
*)
open! Prelude

module type KEYS = sig
  type elt
  type t
  val fold: (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc
end

module type VALUES = sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool

  val union: t -> t -> t
  val empty: t
end

module type MAP = sig
  include Map.CORE
  val update: elt -> ('a option -> 'a option) -> 'a t -> 'a t
  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter: (elt -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val diff:  (elt -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
end

module type S = sig
  type keys
  type values
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool 

  val empty: t
  val is_empty: t -> bool
  val singleton: keys -> values -> t
  val add: keys -> values -> t -> t
  val union: t -> t -> t

  val find: keys -> t -> values
  val find_or: default:values -> keys -> t -> values
end

module Make(M: MAP)(K: KEYS with type elt = M.elt)(V: VALUES) = struct
  type keys = K.t
  type values = V.t
  type t = V.t M.t

  let compare = M.compare V.compare
  let equal = M.equal V.equal

  let empty = M.empty
  let is_empty = M.is_empty

  let add ks vs t =
    K.fold (fun k -> M.add k vs) t ks

  let union t1 t2 =
    M.union (fun _ -> V.union) t1 t2

  let find ks t =
    K.fold (fun k -> V.union @@ M.find k t) V.empty ks

  let singleton ks vs =
    add ks vs empty

  let find_or ~default ks t =
    try find ks t
    with Not_found -> default
end
