open! Prelude
(*
   A set that supports an compliment operation.
   The assumption is that the universe contains all conceivable elements (and is thus infinite).
*)

module type SET = sig
  type elt
  type t

  include Set.CORE with type elt := elt and type t := t
  include Set.BINARY with type elt := elt and type t := t
  val pp: elt Fmt.t -> t Fmt.t

  val subset: t -> t -> bool
  val disjoint: t -> t -> bool
end

module type S = sig
  type elt
  type t

  include Set.CORE with type elt := elt and type t := t
  include Set.BINARY with type elt := elt and type t := t
  include Set.SEQUENTIAL with type elt := elt and type t := t
  val pp: elt Fmt.t -> t Fmt.t

  exception Infinite

  val universe: t
  val comp: t -> t
  val is_finite: t -> bool

  val subset: t -> t -> bool
  val disjoint: t -> t -> bool

  val to_seq_opt: t -> elt Seq.t option
  val to_list_opt: t -> elt list option
end

module Make(S: SET)
  : S with type elt = S.elt =
struct
  type elt = S.elt
  type t = (bool * S.t)

  exception Infinite

  let equal s1 s2 =
    match s1, s2 with
    | (false, s1), (false, s2) -> S.equal s1 s2
    | (true, s1), (true, s2) -> S.equal s1 s2
    | (true, _), (false, _) -> false
    | (false, _), (true, _) -> false

  let compare s1 s2 =
    match s1, s2 with
    | (false, s1), (false, s2) -> S.compare s1 s2
    | (true, s1), (true, s2) -> S.compare s2 s1
    | (true, _), (false, _) -> 1
    | (false, _), (true, _) -> -1

  let pp pp_elt fmt = function
    | (true, s) -> Fmt.brackets Fmt.(const string "^" ++ S.pp pp_elt) fmt s
    | (false, s) -> Fmt.brackets (S.pp pp_elt) fmt s

  let comp (comp, s) = (not comp, s)
  let is_finite (comp, _) = not comp

  let union s1 s2 =
    match s1, s2 with
    | (false, s1), (false, s2) -> (false, S.union s1 s2)
    | (true, s1), (true, s2) -> (true, S.inter s1 s2)
    | (true, s1), (false, s2) -> (true, S.diff s1 s2)
    | (false, s1), (true, s2) -> (true, S.diff s2 s1)

  let inter s1 s2 =
    match s1, s2 with
    | (false, s1), (false, s2) -> (false, S.inter s1 s2)
    | (true, s1), (true, s2) -> (true, S.union s1 s2)
    | (true, s1), (false, s2) -> (false, S.diff s2 s1)
    | (false, s1), (true, s2) -> (false, S.diff s1 s2)

  let disjoint s1 s2 =
    match s1, s2 with
    | (false, s1), (false, s2) -> S.disjoint s1 s2
    | (true, s1), (true, s2) -> S.disjoint s1 s2
    | (true, s1), (false, s2) -> not (S.disjoint s1 s2)
    | (false, s1), (true, s2) ->  not (S.disjoint s1 s2)

  let subset s1 s2 =
    match s1, s2 with
    | (false, s1), (false, s2) -> S.subset s1 s2
    | (true, s1), (true, s2) -> S.subset s2 s1
    | (true, _), (false, _) -> false
    | (false, s1), (true, s2) ->  not (S.subset s1 s2)

  let diff s1 s2 = inter s1 (comp s2)

  let empty = (false, S.empty)
  let singleton elt = (false, S.singleton elt)
  let universe = (true, S.empty)

  let add elt = function
    | (false, s) -> (false, S.add elt s)
    | (true, s) -> (true, S.remove elt s)

  let remove elt = function
    | (false, s) -> (false, S.remove elt s)
    | (true, s) -> (true, S.add elt s)

  let is_empty = function
    | (false, s) -> S.is_empty s
    | (true, _) ->  false

  let fold f seed = function
    | (false, s) -> S.fold f seed s
    | (true, _) -> raise Infinite

  include Set.Sequential(struct
      type nonrec t = t
      type nonrec elt = elt
      let add = add
      let empty = empty
      let fold = fold
    end)

  let to_seq_opt s =
    try Some (to_seq s) with Infinite -> None

  let to_list_opt s =
    try Some (to_list s) with Infinite -> None

  let mem elt = function
    | (false, s) -> S.mem elt s
    | (true, s) -> not (S.mem elt s)

  let the = function
    | (false, s) -> S.the s
    | (true, _) -> raise Infinite
end
