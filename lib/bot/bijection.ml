module type MAP = sig
  include Map.CORE 
end

module type S = sig
  type 'a t
  type elt

  val empty: _ t
  val add: elt -> elt -> 'a -> 'a t -> 'a t
  val remove: elt -> 'a t -> 'a t
end

module Make(M: MAP): S with type elt = M.elt = struct
  type 'a t = {left: 'a M.t; right: 'a M.t}
  type elt = M.elt

  let empty =
    {
      left = M.empty;
      right = M.empty;
    }

  let add x y p t =
    {
      left = M.add x p t.left;
      right = M.add y p t.right
    }

  let remove x t =
    {
      left = M.remove x t.left;
      right = M.remove x t.right;
    }
end
