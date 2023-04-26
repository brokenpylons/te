module type SET = sig
  type t

  val empty: t
  val is_empty: t -> bool
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
end

module type S = sig
  type set
  type t

  val refine: set Seq.t -> t
  val default: t -> set
  val partitions: t -> set Seq.t 
end

module Make(S: SET): S with type set = S.t = struct
  type set = S.t
  type t = {partitions: S.t list; default: S.t}

  let split s p =
    let i = S.inter s p in
    if S.is_empty i then
      `Disjoint
    else
      let d = S.diff p i in
      if S.is_empty d then
        `Equal
      else 
        `Split (i, d)

  let empty =
    {
      partitions = [];
      default = S.empty;
    }

  let partitions t = List.to_seq t.partitions 

  let default t = t.default

  let add s t =
    {
      partitions =
        S.diff s t.default :: List.concat_map (fun p ->
            match split s p with
            | `Disjoint -> [p]
            | `Equal -> [p]
            | `Split (i, d) -> [i; d])
          t.partitions;
      default = S.union s t.default;
    }

  let refine xs =
    Seq.fold_left (Fun.flip add) empty xs
end
