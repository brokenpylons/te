let (%>) f g x = g (f x)

let (%) f g x = f (g x)

let (%%) f g x y = f (g x) (g y)

let (!!) x = Option.get x

let (|?) x y = match x with Some x -> x | None -> y

let ( *** ) f g (x, y) = (f x, g y)

let (&&&) f g x = (f x, g x)

let (@@@) f x = (f x) x

let undefined = Obj.magic 0

module List = struct
  include List

  let the = function
  | [x] -> x
  | _ -> assert false
end

module Seq = struct
  include Seq

  let (@) = Seq.append
end
