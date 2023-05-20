
module type CORE = sig
  type elt
  type 'a t

  val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val is_empty: 'a t -> bool
  val empty: 'a t
  val singleton: elt -> 'a -> 'a t
  val the: 'a t -> (elt * 'a)
  val add: elt -> 'a -> 'a t -> 'a t
  val remove: elt -> 'a t -> 'a t
  val fold: (elt -> 'a -> 'acc -> 'acc) -> 'acc -> 'a t -> 'acc

  val find: elt -> 'a t -> 'a
  val mem: elt -> 'a t -> bool
end

