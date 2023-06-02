type t = Finite of int | Infinity

exception Undefined

let of_int = function
  | n when n >= 0 -> Some (Finite n)
  | _ -> None

let to_int = function
  | Finite n -> Some n
  | Infinity -> None

let to_string = function
  | Finite n -> string_of_int n
  | Infinity -> "∞"

let pp = Fmt.of_to_string to_string

let infinity = Infinity

let succ = function
  | Finite n -> Finite (succ n)
  | Infinity -> Infinity

let max u v =
  match u, v with
  | Finite m, Finite n -> Finite (max m n)
  | _ -> Infinity

let min u v =
  match u, v with
  | Finite m, Finite n -> Finite (min m n)
  | Finite _, Infinity -> u
  | Infinity, Finite _ -> v
  | Infinity, Infinity -> Infinity

let (+) u v =
  match u, v with
  | Finite m, Finite n -> Finite (m + n)
  | _, _ -> Infinity

let ( * ) u v =
  match u, v with
  | Finite m, Finite n -> Finite (m * n)
  | _, _ -> Infinity

let (-) u v =
  match u, v with
  | Finite m, Finite n when m >= n -> Finite (m - n) 
  | Infinity, Finite _ -> Infinity
  | _, _ -> raise Undefined

let (/) u v =
  match u, v with
  | Finite m, Finite n -> Finite (m / n) 
  | Infinity, Finite _ -> Infinity
  | _, _ -> raise Undefined

let (=) u v = 
  match u, v with
  | Finite m, Finite n -> m = n
  | Infinity, Infinity -> true
  | _, _ -> false

let (<) u v =
  match u, v with
  | Finite m, Finite n -> m < n
  | Finite _, Infinity -> true
  | _, _ -> false

let (>) u v =
  match u, v with
  | Finite m, Finite n -> m > n
  | Infinity, Finite _ -> true
  | _, _ -> false

let equal = (=)
let compare u v = 
  if u < v then -1
  else if u > v then 1
  else 0

let (<>) u v = not (u = v)
let (>=) u v = not (u < v)
let (<=) u v = not (u > v)

let finite = function
  | Finite _ -> true
  | Infinity -> false

let infinite u = not (finite u)
