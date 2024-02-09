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

module Make(M: MAP)(K: KEYS with type elt = M.elt)(V: VALUES): S with type t = V.t M.t and type keys = K.t and type values = V.t
