let (%>) f g x = g (f x)

let (%) f g x = f (g x)

let (%%) f g x y = f (g x y)

let curry f x y = f (x, y)

let uncurry f (x, y) = f x y

let on f g x y = f (g x) (g y)

let (!!) x = Option.get x

let (|?) x y = match x with Some x -> x | None -> y

let ( *** ) f g (x, y) = (f x, g y)

let (&&&) f g x = (f x, g x)

let (@@@) f x = (f x) x

let swap (x, y) = (y, x)

let apply2 f g (x1, y1) (x2, y2) = (f x1 x2, g y1 y2)

let undefined = Obj.magic 0

let pp_if b pp ppf  =
  if b then Fmt.pf ppf "%a@ " pp else Fmt.nop ppf

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

  let guard p =
    if p then Seq.return () else Seq.empty

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

  let shortlex cmp xs ys =
    let c = Int.compare (Seq.length xs) (Seq.length ys) in
    if c <> 0 then c else
      Seq.compare cmp xs ys

  let compare_heads cmp xs ys =
    match Seq.uncons xs, Seq.uncons ys with
    | None, None -> 0
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some (x, _), Some (y, _) -> cmp x y

  let rec insert cmp r1 t =
    match Seq.uncons t with
    | None -> Seq.return r1
    | Some (r2, t) ->
      let c = compare_heads cmp r1 r2 in
      if c <= 0
      then Seq.cons r1 (Seq.cons r2 t)
      else Seq.cons r2 (insert cmp r1 t)

  let rec fair_flatten cmp t =
    match Seq.uncons t with
    | None -> Seq.empty
    | Some (r, t) ->
      match Seq.uncons r with
      | None -> fair_flatten cmp t
      | Some (corner, r) ->
        let t = insert cmp r t in
        fun () -> Seq.Cons (corner, fair_flatten cmp t)

  let rec diff cmp d r =
    match Seq.uncons d with
    | None -> r
    | Some (head, d) ->
      let rec go r () =
        match Seq.uncons r with
        | None -> Seq.Nil
        | Some (x, r) ->
          let c = cmp head x in
          if c > 0 then
            Seq.Cons (x, go r)
          else
            diff cmp d r ()
      in go r

  let rec deduplicate cmp r () =
    match Seq.uncons r with
    | None -> Seq.Nil
    | Some (head, r) ->
      Seq.Cons (head, deduplicate cmp (Seq.drop_while (fun x -> cmp head x = 0) r))
end
