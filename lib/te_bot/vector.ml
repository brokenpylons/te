open! Prelude 

type z = Z
type 'n s = S

type ('a, 'n) t =
  | []: ('a, z) t
  | (::): 'a * ('a, 'n) t -> ('a, 'n s) t

let cons x xs = x :: xs

let rec map: type a b n. (a -> b) -> (a, n) t -> (b, n) t = fun f v ->
  match v with
  | [] -> []
  | (x :: xs) -> f x :: map f xs

let rec fold_left: type a n. ('acc -> a -> 'acc) -> 'acc -> (a, n) t -> 'acc = fun f acc v ->
  match v with
  | [] -> acc
  | (x :: xs) -> fold_left f (f acc x) xs

let rec fold_left_map: type a b n. ('acc -> a -> 'acc * b) -> 'acc -> (a, n) t -> 'acc * (b, n) t = fun f acc v ->
  match v with
  | [] -> acc, []
  | (x :: xs) -> 
    let acc, x = f acc x in
    let acc, xs = fold_left_map f acc xs in
    acc, x :: xs

let rec to_seq: type a n. (a, n) t -> a Seq.t = fun v ->
  match v with
  | [] -> Seq.empty
  | (x :: xs) -> Seq.cons x (to_seq xs)

let rec to_list: type a n. (a, n) t -> a list = fun v ->
  match v with
  | [] -> []
  | (x :: xs) -> (x :: to_list xs)

