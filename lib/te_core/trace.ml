module T = Types

module Entry = struct
  type t =
    | Reduce of T.Labeled_var.t * T.Vertex.t * T.Vertex.t * T.Vertex.t
    | Shift of T.Labeled_var.t * T.Vertex.t * T.Vertex.t * T.Vertex.t
    | Read of T.Symbol.t * T.Vertex.t * T.Vertex.t * T.Vertex.t
    | Load of T.Symbol.t * T.Vertex.t * T.Vertex.t
    | Expand of T.Vertex.t * T.Vertex.t
    | Order of T.Vertex.t * T.Var.t
    | Predict of T.Vertex.t * T.Var.t list
  [@@deriving eq, ord]

  let pp ppf = function
    | Reduce (x, v, w, u) -> Fmt.pf ppf "[REDUCE %a: %a -> ... -> %a <- %a]" T.Labeled_var.pp x T.Vertex.pp v T.Vertex.pp w T.Vertex.pp u
    | Shift (x, w, v', u) -> Fmt.pf ppf "[SHIFT %a: %a ~> %a <- %a]" T.Labeled_var.pp x T.Vertex.pp w T.Vertex.pp v' T.Vertex.pp u 
    | Read (x, v, w, u) -> Fmt.pf ppf "[READ '%a': (%a <- %a) <- %a]" T.Symbol.pp x T.Vertex.pp v T.Vertex.pp w T.Vertex.pp u
    | Load (x, v, u) -> Fmt.pf ppf "[LOAD '%a': %a <- %a]" T.Symbol.pp x T.Vertex.pp v T.Vertex.pp u
    | Expand (v, u) -> Fmt.pf ppf "[EXPAND (%a <- %a)]" T.Vertex.pp v T.Vertex.pp u
    | Order (v, x) -> Fmt.pf ppf "[ORDER %a ~ %a ]" T.Vertex.pp v T.Var.pp x
    | Predict (v, xs) -> Fmt.pf ppf "[PREDICT %a ~ %a ]" T.Vertex.pp v (Fmt.list T.Var.pp) xs
end

type t = Entry.t list
[@@deriving eq, ord]

let reduce x v w u = Entry.Reduce (x, v, w, u)
let shift x w v' u = Entry.Shift (x, w, v', u)
let read x v w u = Entry.Read (x, v, w, u)
let load x v u = Entry.Load (x, v, u)
let expand v u = Entry.Expand (v, u)
let order v x = Entry.Order (v, x)
let predict v xs = Entry.Predict (v, xs)

let pp = Fmt.vbox (Fmt.list ~sep:Fmt.cut Entry.pp)

(*let testable = Alcotest.testable pp (List.equal Entry.equal)*)
