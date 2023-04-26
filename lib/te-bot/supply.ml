type 'a t = {value: 'a Lazy.t; left: 'a t Lazy.t; right: 'a t Lazy.t}

let make seed next =
  let update k =
    let v = !k in
    k := next v;
    v
  in
  let rec gen k = 
    {value = lazy (update k); left = lazy (gen k); right = lazy (gen k)}
  in
  gen (ref seed)

let numbers () = make 0 ((+) 1)

let rec map f t =
  {value = lazy (f (Lazy.force t.value)); left = lazy (map f (Lazy.force t.left)); right = lazy (map f (Lazy.force t.right))}

let value t =
  Lazy.force t.value

let get t =
  (Lazy.force t.value, Lazy.force t.left)

let split t =
  Seq.unfold (fun t -> Some (Lazy.force t.left, Lazy.force t.right)) t

let split2 t =
  (Lazy.force t.left, Lazy.force t.right)

let split3 t =
  let p = Lazy.force t.right in
  (Lazy.force t.left, Lazy.force p.left, Lazy.force p.right)

let split4 t =
  let p = Lazy.force t.left in
  let q = Lazy.force t.right in
  (Lazy.force p.left, Lazy.force p.right, Lazy.force q.left, Lazy.force q.right)
