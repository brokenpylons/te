open! Prelude

module type SET = sig
  type t
  type elt

  val is_empty: t -> bool
  val empty: t
  val union: t -> t -> t
  val diff: t -> t -> t
  val fold: (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc
end

module Make(S: SET) = struct
  open S

  let flat_map f t =
    fold (union % f) empty t

  let closure f seed =
    let rec go s x =
      let y = flat_map f x in 
      let s' = union y s in
      let x' = diff y s in
      if is_empty x'
      then s'
      else go s' x'
    in
    go seed seed
end
