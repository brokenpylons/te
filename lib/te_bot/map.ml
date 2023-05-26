open! Prelude 

module type SEQUENTIAL = sig
  type elt 
  type 'a t

  val (&): elt * 'a -> 'a t -> 'a t

  val add_seq: (elt * 'a) Seq.t -> 'a t -> 'a t
  val add_list: (elt * 'a) list -> 'a t -> 'a t

  val of_seq: (elt * 'a) Seq.t -> 'a t
  val of_list: (elt * 'a) list -> 'a t

  val to_seq: 'a t -> (elt * 'a) Seq.t
  val to_list: 'a t -> (elt * 'a) list
end

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

module Sequential(Map: sig
    type elt 
    type _ t
    val add: elt -> 'a -> 'a t -> 'a t
    val empty: 'a t
    val fold: (elt -> 'a -> 'acc -> 'acc) -> 'acc -> 'a t -> 'acc
  end): SEQUENTIAL with type elt := Map.elt and type 'a t := 'a Map.t = struct
  open Map

  let (&) (x, xp) t = add x xp t

  let add_seq s t =
    Seq.fold_left (fun t (x, xp) -> add x xp t) t s

  let of_seq s = 
    add_seq s empty

  let add_list s t =
    List.fold_left (fun t (x, xp) -> add x xp t) t s

  let of_list s = 
    add_list s empty

  let to_list t =
    fold (fun x xp -> List.cons (x, xp)) [] t

  let to_seq t =
    fold (fun x xp -> Seq.cons (x, xp)) Seq.empty t
end
