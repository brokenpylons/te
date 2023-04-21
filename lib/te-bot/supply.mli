(* Based on "On generating unique names" by Lennart Augustsson

   The supply is referentially transparent even though it uses a hidden counter.
   As a result, reusing the supply always results in the same values. *)

type 'a t

val make: 'a -> ('a -> 'a) -> 'a t

val make_number: int t

val numbers_clean: unit -> int t

val map: ('a -> 'b) -> 'a t -> 'b t

val value: 'a t -> 'a

val get: 'a t -> 'a * 'a t

val split: 'a t -> 'a t Seq.t
val split2: 'a t -> 'a t * 'a t
val split3: 'a t -> 'a t * 'a t * 'a t
val split4: 'a t -> 'a t * 'a t * 'a t * 'a t
