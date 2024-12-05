open Te_bot
open! Prelude

let eof_string = "💰"
let delegate_string = "☞"

let explode s =
  let rec go i =
    if i >= String.length s then
      []
    else
      let d = String.get_utf_8_uchar s i in
      if Uchar.utf_decode_is_valid d
      then (Uchar.to_int @@ Uchar.utf_decode_uchar d) :: (go (i + Uchar.utf_decode_length d))
      else assert false
  in go 0

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

  let of_int = singleton

  let of_int_list l =
    List.fold_right add l empty 

  let of_string s =
    of_list (explode s)
end

module State = struct
  include Int
  let to_id x = Dot.(Int x)
  let pp = Fmt.int
  let supply = Supply.numbers ()
  let fresh_supply = Supply.numbers
end

module States = struct
  include Balanced_binary_tree.Set.Size(State)
  let pp = pp State.pp 

  let to_id x = Dot.(String (Fmt.str "%a" pp x))
end
module State_to = struct
  include Balanced_binary_tree.Map.Size(State)
  let pp pp_p = pp State.pp pp_p
end
module State_graph = Graph.Make(State)(State_to)(State_to)

module Statess = struct
  include Balanced_binary_tree.Set.Size(States)
  let pp = pp States.pp 
end
module States_to = struct
  include Balanced_binary_tree.Map.Size(States)
  let pp pp_p = pp States.pp pp_p
end
module States_graph = Graph.Make(States)(States_to)(States_to)

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

module Preserve_state_index = struct
  type elt = States.t
  type t = State.t States_to.t * State.t Supply.t

  let update elt (m, s) =
    try `Old (States_to.find elt m)
    with Not_found ->
      if States.cardinal elt = 1 then
        let n = States.the elt in
        `New (n, elt, (States_to.add elt n m, s))
      else
        let n, s = Supply.get s in
        `New (n, elt, (States_to.add elt n m, s))

  let make s = (States_to.empty, s)
end

module Vertex = struct
  type t = State.t * int
  [@@deriving eq, ord]

  let make state position = (state, position)

  let states = fst

  let position = snd

  let pp ppf (state, position) =
    Fmt.pf ppf "%a:%i" State.pp state position

  let to_id x = Dot.(String (Fmt.str "%a" pp x))
end

module Vertices = struct
  include Balanced_binary_tree.Set.Size(Vertex)
  let pp = pp Vertex.pp 
end
module Vertex_to = struct
  include Balanced_binary_tree.Map.Size(Vertex)
  let pp pp_p = pp Vertex.pp pp_p
end
module Vertex_graph = Graph.Make(Vertex)(Vertex_to)(Vertex_to)

module Edge = struct
  type t = Vertex.t * Vertex.t
  [@@deriving eq, ord]

  let pp ppf (x, y) = 
    Fmt.pf ppf "@[%a <- %a@]" Vertex.pp x Vertex.pp y
end

module Edges = struct
  include Balanced_binary_tree.Set.Size(Edge)
  let pp = pp Edge.pp 
end
module Edge_to = Balanced_binary_tree.Map.Size(Edge)

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

module State_pair_to = struct
  include Balanced_binary_tree.Map.Size(State_pair)
  let pp pp_p = pp State_pair.pp pp_p
end
module State_pair_graph = Graph.Make(State_pair)(State_pair_to)(State_pair_to)

module Partial = struct
  let union x y =
    match x, y with
    | Some _, Some _ -> assert false
    | Some x, None
    | None, Some x -> Some x
    | None, None -> None

  let empty = None
end

module State_partial = struct
  type t = State.t option
  [@@deriving ord, eq]

  let pp ppf = function
    | Some x -> State.pp ppf x
    | None -> Fmt.string ppf "✖"

  include Partial
end

module State_pair_partial = struct
  type t = State_pair.t option
  [@@deriving ord, eq]

  let pp ppf = function
    | Some x -> State_pair.pp ppf x
    | None -> Fmt.string ppf "✖"

  include Partial
end

module States_partial = struct
  type t = States.t option
  [@@deriving ord, eq]

  let pp ppf = function
    | Some x -> States.pp ppf x
    | None -> Fmt.string ppf "✖"

  include Partial
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

  let dummy = (-1, None)

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

  let label = fst
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

module Reduction = struct
  module Strategy = struct
    type t = Null | Fixed of int | Scan of State_pair.t
    [@@deriving eq, ord]

    let pp ppf = function
      | Null -> Fmt.pf ppf "NULL"
      | Fixed d -> Fmt.pf ppf "FIXED %i" d
      | Scan (s, q) -> Fmt.pf ppf "SCAN (%a, %a)" State.pp s State.pp q
  end
  module Reminder = struct
    type t = Complete | Lists of Var.t list list | Gen of State_pair.t
    [@@deriving eq, ord]
  end
  type t = {output: Labeled_var.t; strategy: Strategy.t; reminder: Reminder.t}
  [@@deriving eq, ord]

  let make output strategy reminder = {output; strategy; reminder}

  let pp ppf x =
    Fmt.pf ppf "[%a: %a]" Labeled_var.pp x.output Strategy.pp x.strategy
end
module Reductions = struct
  include Balanced_binary_tree.Set.Size(Reduction)
  let pp = pp Reduction.pp
end

module Symbol = struct
  type t = Delegate | Eof | Code of Code.t | Var of Var.t
  [@@deriving eq, ord]

  let pp ppf = function
    | Eof -> Fmt.string ppf eof_string
    | Delegate -> Fmt.string ppf delegate_string
    | Code x -> Code.pp ppf x
    | Var x -> Var.pp ppf x
end

module Symbol_to = struct
  include Balanced_binary_tree.Map.Size(Symbol)
  let pp pp_p = pp Symbol.pp pp_p
end

module Symbols = struct
  type t =
    {
      eof: bool;
      delegate: bool;
      vars: Vars.t;
      codes: Codes.t
    }
  type elt = Symbol.t

  let empty =
    {
      eof = false;
      delegate = false;
      vars = Vars.empty;
      codes = Codes.empty;
    }

  let add_eof t =
    {t with eof = true}

  let add_delegate t =
    {t with delegate = true}

  let add_vars x t =
    {t with vars = x}

  let add_codes x t =
    {t with codes = x}

  let is_eof x= x.eof
  let is_delegate x= x.delegate
  let to_vars x = x.vars
  let to_codes x = x.codes

  let eof = add_eof empty
  let delegate = add_delegate empty
  let of_vars x = add_vars x empty
  let of_codes x = add_codes x empty

  let add x t =
    match x with
    | Symbol.Delegate -> add_delegate t
    | Symbol.Eof -> add_eof t
    | Symbol.Var x -> add_vars (Vars.singleton x) t
    | Symbol.Code x -> add_codes (Codes.singleton x) t

  let singleton x = 
    add x empty

  let pp_if b pp ppf  =
    if b then Fmt.pf ppf "%a@ " pp else Fmt.nop ppf

  let pp ppf x =
    Fmt.pf ppf "@[%a%a%a%a@]"
      (pp_if x.eof Fmt.string) eof_string
      (pp_if x.delegate Fmt.string) delegate_string
      (pp_if (not @@ Vars.is_empty x.vars) Vars.pp) x.vars
      (pp_if (not @@ Codes.is_empty x.codes) Codes.pp) x.codes

  let subset x y =
    Bool.imp x.eof y.eof &&
    Bool.imp x.delegate y.delegate &&
    Vars.subset x.vars y.vars &&
    Codes.subset x.codes y.codes

  let union x y =
    {
      eof = x.eof || y.eof;
      delegate = x.delegate || y.delegate;
      vars = Vars.union x.vars y.vars;
      codes = Codes.union x.codes y.codes;
    }

  let inter x y =
    {
      eof = x.eof && y.eof;
      delegate = x.delegate && y.delegate;
      vars = Vars.inter x.vars y.vars;
      codes = Codes.inter x.codes y.codes;
    }

  let diff x y =
    {
      eof = not (Bool.imp x.eof y.eof);
      delegate = not (Bool.imp x.delegate y.delegate);
      vars = Vars.diff x.vars y.vars;
      codes = Codes.diff x.codes y.codes;
    }
end

module type SYMBOL_MULTIMAP_VALUES = sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool

  val empty: t
  val union: t -> t -> t
end

module Symbols_multimap(Values: SYMBOL_MULTIMAP_VALUES) = struct
  module Vars_multimap = Relation.Make(Var_to)(Vars)(Values)
  module Codes_multimap = Refine.Map(Codes)(Values)

  type key = Symbols.t
  type values = Values.t
  type t = {eof: Values.t; delegate: Values.t; vars: Vars_multimap.t; codes: Codes_multimap.t}
  [@@deriving eq, ord]

  let pp pp_values ppf x =
    Fmt.pf ppf "@[@[%a@]@,@[%a@]@,@[%a@]@,@[%a@]@]"
      pp_values x.eof
      pp_values x.delegate
      (Var_to.pp pp_values) x.vars
      (Fmt.seq (Fmt.pair Codes.pp pp_values)) (Codes_multimap.partitions x.codes)

  let empty =
    {
      eof = Values.empty;
      delegate = Values.empty;
      vars = Vars_multimap.empty;
      codes = Codes_multimap.empty;
    }

  let add_multiple k vs t =
    {
      eof = if k.Symbols.eof then Values.union vs t.eof else t.eof;
      delegate = if k.Symbols.delegate then Values.union vs t.delegate else t.delegate;
      vars = Vars_multimap.add k.vars vs t.vars;
      codes = Codes_multimap.add k.codes vs t.codes;
    }

  let singleton_multiple k vs = add_multiple k vs empty

  let find_multiple k t =
    let (<|>) = Values.union in
    ((if k.Symbols.eof then t.eof else Values.empty)
     <|> (if k.Symbols.delegate then t.delegate else Values.empty)
     <|> Vars_multimap.find k.vars t.vars
     <|> Codes_multimap.find k.codes t.codes)

  let find_multiple_or_empty k t =
      try find_multiple k t
      with Not_found -> Values.empty

  let union x y =
    {
      eof = Values.union x.eof y.eof;
      delegate = Values.union x.delegate y.delegate;
      vars = Vars_multimap.union x.vars y.vars;
      codes = Codes_multimap.union x.codes y.codes;
    }

  let (<|>) = union

  let add_seq_multiple s t =
    Seq.fold_left (fun t (k, v) -> add_multiple k v t) t s

  let of_seq_multiple s =
    add_seq_multiple s empty
end

module Node = struct
  type t = Symbol.t * int * int
  [@@deriving eq, ord]

  let make state start stop = (state, start, stop)
  let symbol (v, _, _) = v

  let pp ppf (v, start, stop) =
    Fmt.pf ppf "%a:%i:%i" Symbol.pp v start stop

  let to_id x = Dot.(String (Fmt.str "%a" pp x))
end
module Nodes = struct
  include Balanced_binary_tree.Set.Size(Node)
  let pp = pp Node.pp
end
module Node_to = Balanced_binary_tree.Map.Size(Node)
module Node_packed_forest = Packed_forest.Make(Node)(Vars)(Node_to)

module Actions = struct
  type t =
    {
      accept: bool;
      shift: bool;
      load: bool;
      orders: Vars.t;
      matches: Labeled_vars.t;
      predictions: Vars.t;
      null: Reductions.t;
      reduce: Reductions.t;
    }
  [@@deriving eq, ord]

  let pp ppf x =
    Fmt.pf ppf "@[%a%a%a%a%a%a@]"
      (pp_if x.load Fmt.string) "LOAD"
      (pp_if (not @@ Vars.is_empty x.orders) (fun ppf -> Fmt.pf ppf "ORDER %a" Vars.pp)) x.orders
      (pp_if (not @@ Labeled_vars.is_empty x.matches) (fun ppf -> Fmt.pf ppf "MATCH %a" Labeled_vars.pp)) x.matches
      (pp_if (not @@ Vars.is_empty x.predictions) (fun ppf -> Fmt.pf ppf "PRED %a" Vars.pp)) x.predictions
      (pp_if (not @@ Reductions.is_empty x.null) Reductions.pp) x.null
      (pp_if (not @@ Reductions.is_empty x.reduce) Reductions.pp) x.reduce

  let union x y =
    {
      accept = x.accept || y.accept;
      shift = x.shift || y.shift;
      load = x.load || y.load;
      orders = Vars.union x.orders y.orders;
      matches = Labeled_vars.union x.matches y.matches;
      predictions = Vars.union x.predictions y.predictions;
      null = Reductions.union x.null y.null;
      reduce = Reductions.union x.reduce y.reduce;
    }

  let empty =
    {
      accept = false;
      shift = false;
      load = false;
      orders = Vars.empty;
      matches = Labeled_vars.empty;
      predictions = Vars.empty;
      null = Reductions.empty;
      reduce = Reductions.empty;
    }

  let is_empty x =
    not x.accept &&
    not x.shift &&
    Vars.is_empty x.orders &&
    Labeled_vars.is_empty x.matches &&
    Vars.is_empty x.predictions &&
    Reductions.is_empty x.null &&
    Reductions.is_empty x.reduce

  let accept =
    {empty with accept = true}

  let shift =
    {empty with shift = true}

  let load =
    {empty with load = true}

  let orders x =
    {empty with orders = x}

  let matches x =
    {empty with matches = x}

  let predictions x =
    {empty with predictions = x}

  let null x =
    {empty with null = x}

  let reduce x =
    {empty with reduce = x}
end
