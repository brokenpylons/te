open Te_bot

module Code = struct
  include Int 
  let to_id x = Dot.(Int x)
  let pp = Fmt.int
end
module Codes = struct
  include Balanced_binary_tree.Set.Size(Code)
  let pp = pp Code.pp
end

module State = struct
  include Int
  let to_id x = Dot.(Int x)
  let pp = Fmt.int
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
  type t = int * string option
  [@@deriving ord, eq]

  let make ?label x = (x, label)

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
