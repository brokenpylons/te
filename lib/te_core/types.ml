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

module States = struct
  include Balanced_binary_tree.Set.Size(State)
  let pp = pp State.pp 
end
module State_to = struct
  include Balanced_binary_tree.Map.Size(State)
  let pp pp_p = pp State.pp pp_p
end
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

module Vertex = struct
  type t = State.t * int
  [@@deriving eq, ord]

  let make state position = (state, position)

  let state = fst

  let pp ppf (state, position) =
    Fmt.pf ppf "%a:%i" State.pp state position

  let to_id x = Dot.(String (Fmt.str "%a" pp x))
end

module State_pair = struct
  type t = State.t * State.t
  [@@deriving eq, ord]

  let pp = Fmt.parens (Fmt.pair ~sep:(Fmt.const Fmt.string ",") State.pp State.pp)

  let to_id x = Dot.(String (Fmt.str "%a" pp x))
end
module State_pairs = struct
  include Balanced_binary_tree.Set.Size(State_pair)
  let pp = pp State_pair.pp 
end

module State_pair_to = Balanced_binary_tree.Map.Size(State_pair)
module State_pair_graph = Graph.Make(State_pair)(State_pair_to)(State_pair_to)

module State_partial = struct
  type t = State.t option
  [@@deriving ord, eq]

  let pp ppf = function
    | Some x -> State.pp ppf x
    | None -> Fmt.string ppf "✖"

  let union x y =
    match x, y with
    | Some _, Some _ -> assert false
    | Some x, None
    | None, Some x -> Some x
    | None, None -> None

  let empty = None
end

module Vertices = struct
  include Balanced_binary_tree.Set.Size(Vertex)
  let pp = pp Vertex.pp 
end
module Vertex_to = Balanced_binary_tree.Map.Size(Vertex)
module Vertex_graph = Graph.Make(Vertex)(Vertex_to)(Vertex_to)

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
module Var_to = struct
  include Balanced_binary_tree.Map.Size(Var)
  let pp pp_p = pp Var.pp pp_p
end

module Labeled_var = struct
  type t = Var.t * Var.t
  [@@deriving eq, ord] 

  let node = fst
  let var = snd 

  let pp ppf (z, s) =
    Fmt.pf ppf "@[%a. %a@]" Var.pp z Var.pp s
end

module Labeled_vars = struct
  include Balanced_binary_tree.Set.Size(Labeled_var)
  let pp = pp Labeled_var.pp
end

module Labeled_var_to = struct
  include Balanced_binary_tree.Map.Size(Labeled_var)
  let pp pp_p = pp Labeled_var.pp pp_p
end
