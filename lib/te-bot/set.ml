
module type CORE = sig
  type elt 
  type t

  val compare: t -> t -> int
  val equal: t -> t -> bool

  val is_empty: t -> bool
  val empty: t
  val singleton: elt -> t
  val add: elt -> t -> t
  val remove: elt -> t -> t

  val find: elt -> t -> elt
  val mem: elt -> t -> bool
  val the: t -> elt

  val to_seq: t -> elt Seq.t
  val of_seq: elt Seq.t -> t
  val to_list: t -> elt list
  val of_list: elt list -> t
end
