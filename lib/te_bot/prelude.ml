let (%>) f g x = g (f x)

let (%) f g x = f (g x)

let curry f x y = f (x, y)

let uncurry f (x, y) = f x y

let on f g x y = f (g x) (g y)

let (!!) x = Option.get x

let (|?) x y = match x with Some x -> x | None -> y

let ( *** ) f g (x, y) = (f x, g y)

let (&&&) f g x = (f x, g x)

let (@@@) f x = (f x) x

let swap (x, y) = (y, x)

let undefined = Obj.magic 0

let rec until f x =
  match f x with 
  | Some y -> until f y
  | None -> x

module Bool = struct
  include Bool

  let imp x y =
    not x || y

  let not_imp x y =
    x && not y
end

module List = struct
  include List

  let the = function
  | [x] -> x
  | _ -> assert false

  let add_opt x xs =
    match x with
    | Some x -> x :: xs
    | None -> xs

  let bin_map p left right xs =
    let rec go left right = function
      | [] -> (rev left, rev right)
      | x :: xs ->
        let (l, r) = p x in
        go (add_opt l left) (add_opt r right) xs
    in
    go left right xs
end

module Seq = struct
  include Seq

  let (@) = Seq.append

  let fold_left_map f seed s =
    let rec go acc p s =
      match s () with
      | Cons (x, s) ->
        let acc, x = f acc x in
        go acc (Seq.cons x p) s
      | Nil ->
        (acc, p)
    in
    go seed Seq.empty s
end
