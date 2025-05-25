open Te_bot
open! Prelude
module T = Types

module Path = struct
  type t = T.Vertex.t * T.Node.t list
  [@@deriving ord, show]
end

module Paths = struct
 include List
 let singleton x = [x]
 let add x xs = x :: xs
 let empty = []
end

module Gss = struct
  module G = T.Vertex_graph
  type t = (unit, T.Nodes.t) G.t
  let singleton v = G.singleton v ()

  let add u g =
    (*assert (not (G.vertex_mem u g));*)
    G.add u () g

  let connect u v n g =
    G.connect u v n g

  let contains u g =
    G.vertex_mem u g

  let contains_edge u v g =
    G.edge_mem u v g

  let node u v g =
    G.edge_label u v g

  let adjacent v ns g =
    G.adjacent v g
    |> Seq.flat_map (fun (w, n) ->
        T.Nodes.to_seq n
        |> Seq.map (fun n -> (w, (n :: ns))))
    |> Paths.of_seq
end

module Forest = T.Node_packed_forest

module Subclasses = Multimap.Make3(Balanced_binary_tree.Map.Size(Int))(T.Vertices)

module Segments = Multimap.Make3(T.Vertex_to)(T.Vertices)
module Orders = Multimap.Make3(T.Var_to)(T.Vertices)
module Orders' = Multimap.Make3(T.Vertex_to)(Orders.Set)

class type driver = object
  method read: T.Symbol.t -> unit
  method forest: Forest.t
  method trace: Trace.t
  method accept: bool
  method to_dot: Dot.graph
end
