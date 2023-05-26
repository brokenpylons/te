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

  type elt
  val singleton: elt -> t
  val the: t -> elt
  val add: elt -> t -> t
  val fold: (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc

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
end

module Make(M: MAP) = struct

  module L0(S: SET0) = struct
     type t = S.t M.t

     let compare = M.compare S.compare 
     let equal = M.equal S.equal

    let empty = M.empty
    let is_empty = M.is_empty
    let singleton_multiple = M.singleton
    let the_multiple = M.the 
    let fold_multiple = M.fold
    let domain_mem = M.mem

    let to_seq_multiple t =
      fold_multiple (fun x xp -> Seq.cons (x, xp)) Seq.empty t

    let find_multiple = M.find

  end

  module L1(S: SET1) = struct
    include L0(S)

    let add_multiple x xp t =
      M.update x (function Some s -> Some (S.union xp s) | None -> Some xp) t

    let union t1 t2 =
      M.union (fun _ -> S.union) t1 t2

    let (<|>) = union
  end

  module L2(S: SET2) = struct
    include L1(S)
    type elt = M.elt * S.elt

    let singleton k v = M.singleton k (S.singleton v)

    let fold f acc t =
      M.fold (fun x -> (Fun.flip @@ S.fold (fun xp -> f x xp))) acc t

    let add x xp t =
      M.update x (function Some s -> Some (S.add xp s) | None -> Some (S.singleton xp)) t

    include Set.Sequential(struct
        type nonrec t = t
        type nonrec elt = elt
        let add (x, xp) t = add x xp t
        let empty = empty
        let fold f = fold (fun x xp -> f (x, xp))
      end)
  end
end
