module type SEQUENTIAL = sig
  type elt 
  type t

  val (&): elt -> t -> t

  val add_seq: elt Seq.t -> t -> t
  val add_list: elt list -> t -> t

  val of_seq: elt Seq.t -> t
  val of_list: elt list -> t

  val to_seq: t -> elt Seq.t
  val to_list: t -> elt list
end

module type BINARY = sig
  type elt 
  type t

  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
end

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
  val fold: (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc

  val mem: elt -> t -> bool
  val the: t -> elt
end

module Sequential(Set: sig
    type elt 
    type t
    val add: elt -> t -> t
    val empty: t
    val fold: (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  end): SEQUENTIAL with type elt := Set.elt and type t := Set.t = struct
  open Set

  let (&) x t = add x t

  let add_seq s t =
    Seq.fold_left (Fun.flip add) t s

  let of_seq s = 
    add_seq s empty

  let add_list s t =
    List.fold_left (Fun.flip add) t s

  let of_list s = 
    add_list s empty

  let to_list t =
    fold List.cons [] t

  let to_seq t =
    fold Seq.cons Seq.empty t
end
