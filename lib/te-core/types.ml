open Te_bot
open! Prelude

module Code = struct
  include Int 
  let to_id x = Dot.(Int x)
  let of_int = Fun.id

  let to_string x =
    let b = Buffer.create 4 in
    Buffer.add_utf_8_uchar b (Uchar.of_int x);
    Buffer.contents b

  let pp = Fmt.of_to_string to_string
end
module Codes = struct
  include Comp_set.Make(Balanced_binary_tree.Set.Size(Code))
  let pp = pp Code.pp

  let of_string s =
    let rec go i =
      if i >= String.length s then
        empty
      else 
        let d = String.get_utf_8_uchar s i in
        if Uchar.utf_decode_is_valid d
        then add (Uchar.to_int @@ Uchar.utf_decode_uchar d) (go (i + Uchar.utf_decode_length d))
        else assert false
    in go 0
end

module State = struct
  include Int
  let to_id x = Dot.(Int x)
  let pp = Fmt.int
  let supply = Supply.numbers ()
end

module States = Balanced_binary_tree.Set.Size(State)
module State_to = Balanced_binary_tree.Map.Size(State)
module State_graph = Graph.Make(State)(State_to)(State_to)

module type INDEX_MAP = sig
  type elt
  type _ t

  val find: elt -> 'a t -> 'a
  val empty: _ t
  val add: elt -> 'a -> 'a t -> 'a t
end

module State_index(Map: INDEX_MAP) = struct
  type elt = Map.elt
  type t = State.t Map.t * State.t Supply.t

  let update elt (m, s) = 
    try `Old (Map.find elt m)
    with Not_found -> 
      let n, s = Supply.get s in
      `New (n, elt, (Map.add elt n m, s))

  let make s = (Map.empty, s)
end

module Var = struct
  type pre = int
  type t = int * string option
  [@@deriving ord, eq]

  let supply = Supply.numbers ()

  let make ~supply labels = 
    snd @@ Vector.fold_left_map (fun supply label ->
        let (x, supply) = Supply.get supply in
        (supply, (x, Some label)))
      supply labels

  let value_pp ppf = function
    | Some label -> Fmt.string ppf label
    | None -> Fmt.string ppf "?"

  let pp ppf (_, label) =
    Fmt.pf ppf "@[%a@]" value_pp label
end

module Vars = struct
  include Balanced_binary_tree.Set.Size(Var)
  let pp = pp Var.pp
end
module Var_to = Balanced_binary_tree.Map.Size(Var)
