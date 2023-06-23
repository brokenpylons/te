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

  type elt
  val singleton: elt -> t
  val the: t -> elt
  val add: elt -> t -> t
  val fold: (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  val inter: t -> t -> t

  include Set.SEQUENTIAL with type t := t and type elt := elt
end

module type SET3 = sig
  include SET2

  val remove: elt -> t -> t
  val is_empty: t -> bool
end

module type MAP = sig
  include Map.CORE 
  val update: elt -> ('a option -> 'a option) -> 'a t -> 'a t
  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val diff: 'a t -> 'a t -> 'a t
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

  val diff: t -> t -> t
end

module Make0(M: MAP)(S: SET0): S0 with type t = S.t M.t and type key = M.elt and type values = S.t

module type S1 = sig
  include S0

  val add_multiple: key -> values -> t -> t
  val union: t -> t -> t
  val (<|>): t -> t -> t
end

module Make1(M: MAP)(S: SET1): S1 with type t = S.t M.t and type key = M.elt and type values = S.t

module type S2 = sig
  include S1
  type value
  type elt = key * value

  val singleton: key -> value -> t

  val fold: (key -> value -> 'acc -> 'acc) -> 'acc -> t -> 'acc

  val add: key -> value -> t -> t
  val the: t -> key * value

  val inter: t -> t -> t

  include Set.SEQUENTIAL with type t := t and type elt := elt
  module Set: SET2 with type t = t and type elt = elt
end

module Make2(M: MAP)(S: SET2): S2 with type t = S.t M.t and type key = M.elt and type value = S.elt and type values = S.t
