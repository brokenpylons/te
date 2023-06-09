type t = private Bot | Finite of int | Top

exception Undefined

val of_int: int -> t
val to_int: t -> int option
val to_string: t -> string
val pp: t Fmt.t
val zero: t
val top: t
val bot: t

val succ: t -> t
val max: t -> t -> t
val min: t -> t -> t
val (+): t -> t -> t
val (-): t -> t -> t
val ( * ): t -> t -> t
val (/): t -> t -> t

val equal: t -> t -> bool
val compare: t -> t -> int
val (<): t -> t -> bool
val (>): t -> t -> bool
val (<=): t -> t -> bool
val (>=): t -> t -> bool
val (=): t -> t -> bool
val (<>): t -> t -> bool

val is_top: t -> bool
val is_bot: t -> bool

val is_finite: t -> bool
val is_infinite: t -> bool

