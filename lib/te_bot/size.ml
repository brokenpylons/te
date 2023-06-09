(* Natural numbers extended with lower and upper bound *)

type t = Bot | Finite of int | Top

exception Undefined

let of_int n = 
  assert (n >= 0);
  Finite n

let to_int = function
  | Finite n -> Some n
  | Bot -> None
  | Top -> None

let to_string = function
  | Finite n -> string_of_int n
  | Bot -> "⊥"
  | Top -> "⊤"

let pp = Fmt.of_to_string to_string

let zero = Finite 0

let top = Top

let bot = Bot

let succ = function
  | Finite n -> Finite (succ n)
  | Bot -> Bot
  | Top -> Top

let max u v =
  match u, v with
  | Finite m, Finite n -> Finite (max m n)
  | Finite _, Bot ->  u
  | Bot, Finite _ -> v
  | Bot, Bot -> Bot
  | _ -> Top

let min u v =
  match u, v with
  | Finite m, Finite n -> Finite (min m n)
  | Finite _, Top -> u
  | Top, Finite _ -> v
  | Top, Top -> Top
  | _ -> Bot

let (+) u v =
  match u, v with
  | Finite m, Finite n -> Finite (m + n)
  | Top, Finite _
  | Finite _, Top
  | Top, Top -> Top
  | Bot, Finite _
  | Finite _, Bot 
  | Bot, Bot -> Bot
  | _, _ -> raise Undefined

let ( * ) u v =
  match u, v with
  | Finite m, Finite n -> Finite (m * n)
  | Top, Finite n 
  | Finite n, Top when n <> 0 -> Top
  | Top, Top -> Top
  | Bot, Finite n
  | Finite n, Bot when n <> 0 -> Bot
  | Bot, Bot -> Bot
  | _, _ -> raise Undefined

let (-) u v =
  match u, v with
  | Finite m, Finite n when m >= n -> Finite (m - n) 
  | Top, Finite _
  | Finite _, Top
  | Top, Top -> Top
  | Bot, Finite _
  | Finite _, Bot 
  | Bot, Bot -> Bot
  | _, _ -> raise Undefined

let (/) u v =
  match u, v with
  | Finite m, Finite n -> Finite (m / n) 
  | Finite _, Top 
  | Finite _, Bot -> u
  | Top, Finite _ -> Top
  | Bot, Finite _ -> Bot
  | _, _ -> raise Undefined

let (=) u v = 
  match u, v with
  | Finite m, Finite n -> m = n
  | Top, Top -> true
  | Bot, Bot -> true
  | _, _ -> false

let (<) u v =
  match u, v with
  | Finite m, Finite n -> m < n
  | Finite _, Top -> true
  | Bot, _ -> true
  | _, _ -> false

let (>) u v =
  match u, v with
  | Finite m, Finite n -> m > n
  | Finite _, Bot -> true
  | Top, _ -> true
  | _, _ -> false

let equal = (=)
let compare u v = 
  if u < v then -1
  else if u > v then 1
  else 0

let (<>) u v = not (u = v)
let (>=) u v = not (u < v)
let (<=) u v = not (u > v)

let is_finite = function
  | Finite _ -> true
  | Top -> false
  | Bot -> false

let is_top = function
  | Top -> true
  | _ -> false

let is_bot = function
  | Bot -> true
  | _ -> false

let is_infinite u = not (is_finite u)
