open Te_bot
module T = Types

module Lits: sig
  type t = Nothing | Null | Call | Return | Vars of T.Vars.t | Codes of T.Codes.t
  include Re.LITS with type t := t
end = struct
  type t = Nothing | Null | Call | Return | Vars of T.Vars.t | Codes of T.Codes.t
  [@@deriving eq, ord]

  let pp ppf = function
    | Nothing -> Fmt.string ppf "∅"
    | Null -> Fmt.string ppf "ε"
    | Call -> Fmt.string ppf "◀"
    | Return -> Fmt.string ppf "▶"
    | Vars x -> T.Vars.pp ppf x
    | Codes x -> T.Codes.pp ppf x

  let subset x y =
    match x, y with
    | Call, Call -> true
    | Return, Return -> true
    | Vars x, Vars y -> T.Vars.subset x y
    | Codes x, Codes y -> T.Codes.subset x y 
    | _, _ -> false
end

module M = Fa.Make(Lits)
module R = Re.Concrete(Lits)
module R_to = Balanced_binary_tree.Map.Size(R)
module Gen = M.Gen(T.State_index(R_to))

let build ~supply (r: R.t) =
  Gen.unfold ~supply (fun _ r ->
      (R.is_nullable r, (), Seq.map (fun ls -> (ls, r)) @@ R.first r))
    r

