open! Prelude

module type VERTEX = sig
  type t

  val pp: t Fmt.t
  val equal: t -> t -> bool
  val to_id: t -> Dot.id
end

module type VERTEX_MAP = sig
  type elt
  type 'a t

  include Map.CORE with type elt := elt and type 'a t := 'a t

  val modify: elt -> ('a -> 'a) -> 'a t -> 'a t
  val cardinal: 'a t -> int

  val map: (elt -> 'a -> 'b) -> 'a t -> 'b t
  val iter: (elt -> 'a -> unit) -> 'a t -> unit

  val to_seq: 'a t -> (elt * 'a) Seq.t
  val of_seq: (elt * 'a) Seq.t -> 'a t

  val domain_disjoint: 'a t -> 'a t -> bool
  val mem: elt -> _ t -> bool
  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
end

module type EDGE_MAP = sig
  type elt
  type 'a t

  include Map.CORE with type elt := elt and type 'a t := 'a t
  val add_with: (elt -> elt -> elt) -> ('a -> 'a -> 'a) -> elt -> 'a -> 'a t -> 'a t

  val map: (elt -> 'a -> 'b) -> 'a t -> 'b t

  val to_seq: 'a t -> (elt * 'a) Seq.t
  val of_seq: (elt * 'a) Seq.t -> 'a t

  val union: (elt -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
end

module type KLEENE = sig
  type t

  val nothing: t 
  val null: t
  val union: t -> t -> t
  val concat: t -> t -> t
  val start: t
end

module type S = sig
  type vertex
  type 'vl labeled_vertex = (vertex * 'vl)
  type 'el labeled_edge = (vertex * vertex * 'el)
  type 'el adjacent = (vertex * 'el)
  type ('vl, 'el) t

  val equal: ('vl -> 'vl -> bool) -> ('el -> 'el -> bool) -> ('vl, 'el) t -> ('vl, 'el) t -> bool

  val compare: ('vl -> 'vl -> int) -> ('el -> 'el -> int) -> ('vl, 'el) t -> ('vl, 'el) t -> int

  val order: (_, _) t -> int

  val empty: (_, _) t

  val skeleton: (_, _) t -> (unit, unit) t 

  val singleton: vertex -> 'vl -> ('vl, 'el) t

  val add: vertex -> 'vl -> ('vl, 'el) t -> ('vl, 'el) t

  val add_segment: vertex -> 'vl -> (vertex * 'el) Seq.t -> ('vl, 'el) t -> ('vl, 'el) t

  val add_segment_safe: vertex -> 'vl -> (vertex * 'el) Seq.t -> ('vl, 'el) t -> ('vl, 'el) t

  exception Not_disjoint
  val sum: ('vl, 'el) t -> ('vl, 'el) t -> ('vl, 'el) t

  val merge: merge_vertex_label:('vl -> 'vl -> 'vl) -> merge_edge_label:('el -> 'el -> 'el) -> ('vl, 'el) t -> ('vl, 'el) t -> ('vl, 'el) t

  val remove: vertex -> ('vl, 'el) t -> ('vl, 'el) t

  val remove_edge: vertex -> vertex -> ('vl, 'el) t -> ('vl, 'el) t

  val connect: vertex -> vertex -> 'el -> ('vl, 'el) t -> ('vl, 'el) t

  val link: vertex Seq.t -> vertex Seq.t -> 'el -> ('vl, 'el) t -> ('vl, 'el) t

  val link_with: ('el -> 'el -> 'el) -> vertex Seq.t -> vertex Seq.t -> 'el -> ('vl, 'el) t -> ('vl, 'el) t

  val vertex_label: vertex -> ('vl, _) t -> 'vl

  val edge_label: vertex -> vertex -> (_, 'el) t -> 'el

  val vertex_mem: vertex -> (_, _) t -> bool

  val edge_mem: vertex -> vertex -> (_, _) t -> bool

  val adjacent: vertex -> (_, 'el) t -> 'el adjacent Seq.t

  val context: vertex -> ('vl, 'el) t -> 'vl * 'el adjacent Seq.t

  val adjacency_list: ('vl, 'el) t -> ('vl labeled_vertex * 'el adjacent Seq.t) Seq.t

  val to_seq: ('vl, 'el) t -> 'vl labeled_vertex Seq.t * 'el labeled_edge Seq.t

  val of_seq: 'vl labeled_vertex Seq.t -> 'el labeled_edge Seq.t -> ('vl, 'el) t

  val of_edges: (vertex * vertex) Seq.t -> (unit, unit) t

  val of_labeled_edges: (vertex * vertex * 'el) Seq.t -> (unit, 'el) t

  val labeled_vertices: ('vl, _) t -> 'vl labeled_vertex Seq.t 

  val labeled_vertices_map: (vertex -> 'vl -> 'a) -> ('vl, 'el) t -> ('a, 'el) t

  val labeled_edges: (_, 'el) t -> 'el labeled_edge Seq.t

  val edge_labels: (_, 'el) t -> 'el Seq.t

  val labeled_edges_map: (vertex -> vertex -> 'el -> 'a) -> ('vl, 'el) t -> ('vl, 'a) t

  val vertices: (_, _) t -> vertex Seq.t

  val edges: (_, _) t -> (vertex * vertex) Seq.t

  val transpose: ('vl, 'el) t -> ('vl, 'el) t

  val fold: eq:('seed -> 'seed -> bool) -> (vertex -> 'vl -> 'seed) -> ('seed -> 'vl -> ('el * 'seed) Seq.t -> 'seed) -> ('vl, 'el) t -> (vertex -> 'seed)

  val dijkstra: (vertex -> 'vl -> 'seed) -> ('seed -> 'vl -> ('el * (unit -> 'seed)) Seq.t -> 'seed) -> ('vl, 'el) t -> (vertex -> 'seed)

  val digraph: (vertex -> 'vl -> 'seed) -> ('seed -> 'vl -> ('el * (unit -> 'seed)) Seq.t -> 'seed) -> ('vl, 'el) t -> (vertex -> 'seed)

  val unfold: (vertex -> 'seed -> 'vl * ('el * vertex * 'seed) Seq.t) -> vertex -> 'seed -> ('vl, 'el) t

  val tie: ((vertex -> 'result) -> vertex -> 'vl -> (vertex * 'el) Seq.t -> 'result) -> ('vl, 'el) t -> (vertex -> 'result)

  val relabel: (vertex -> vertex) -> ('vl, 'el) t -> ('vl, 'el) t

  module type INDEX = sig
    type t
    type elt
    val update: elt -> t -> [> `New of vertex * elt * t | `Old of vertex]
    val make: vertex Supply.t -> t
  end
  module Gen(Index: INDEX): sig
    val unfold_multiple: supply:(vertex Supply.t) -> ?merge: ('el -> 'el -> 'el) -> (vertex -> Index.elt -> 'vl * ('el * Index.elt) Seq.t) -> Index.elt list -> vertex list * ('vl, 'el) t

    val unfold: supply:(vertex Supply.t) -> ?merge: ('el -> 'el -> 'el) -> (vertex -> Index.elt -> 'vl * ('el * Index.elt) Seq.t) -> Index.elt -> vertex * ('vl, 'el) t
  end

  val pp: 'vl Fmt.t -> 'el Fmt.t -> ('vl, 'el) t Fmt.t
  val to_dot: string_of_vertex_label:('vl -> string) -> string_of_edge_label:('el -> string) -> ('vl, 'el) t -> Dot.graph
end

module Make
    (Vertex: VERTEX)
    (Vertex_map: VERTEX_MAP with type elt = Vertex.t) 
    (Edge_map: EDGE_MAP with type elt = Vertex.t): S with type vertex = Vertex.t = struct
  type vertex = Vertex.t
  type 'vl labeled_vertex = (vertex * 'vl)
  type 'el labeled_edge = (vertex * vertex * 'el)
  type 'el adjacent = (vertex * 'el)
  type ('vl, 'el) ctx = 'vl * 'el Edge_map.t
  type ('vl, 'el) t = (('vl, 'el) ctx) Vertex_map.t

  let equal vl_eq el_eq x y = 
    Vertex_map.equal (fun (vl, adj1) (ul, adj2) ->
        vl_eq vl ul && Edge_map.equal el_eq adj1 adj2)
      x y

  let compare cmp_vl cmp_el x y = 
    Vertex_map.compare (fun (vl, adj1) (ul, adj2) ->
        let c = cmp_vl vl ul in
        if c <> 0 then c
        else Edge_map.compare cmp_el adj1 adj2)
      x y

  let order = Vertex_map.cardinal

  let empty = Vertex_map.empty

  let singleton v vl = Vertex_map.singleton v (vl, Edge_map.empty)

  let add v vl g =
    Vertex_map.add v (vl, Edge_map.empty) g

  (* unsafe *)
  let add_segment v vl adj g =
    Vertex_map.add v (vl, Edge_map.of_seq adj) g

  exception Not_disjoint

  let sum g1 g2 =
    Vertex_map.union (fun _ _ _ -> raise Not_disjoint) g1 g2

  let merge ~merge_vertex_label ~merge_edge_label g1 g2 =
    Vertex_map.union (fun _ (vl1, adj1) (vl2, adj2) -> (merge_vertex_label vl1 vl2, Edge_map.union (fun _ -> merge_edge_label) adj1 adj2)) g1 g2

  let remove v g =
    g 
    |> Vertex_map.remove v
    |> Vertex_map.map (fun _ (vl, adj) -> (vl, Edge_map.remove v adj))

  let remove_edge v u g = 
    Vertex_map.modify v (fun (vl, adj) -> (vl, Edge_map.remove u adj)) g

  let connect v u el g =
    assert (Vertex_map.mem u g);
    Vertex_map.modify v (fun (vl, adj) -> (vl, Edge_map.add u el adj)) g

  let connect_with f v u el g =
    assert (Vertex_map.mem u g);
    Vertex_map.modify v (fun (vl, adj) -> (vl, Edge_map.add_with Fun.const f u el adj)) g

  let link vs us el g =
    Seq.fold_left (fun g (v, u) -> connect v u el g) g (Seq.product vs us)

  let link_with f vs us el g =
    Seq.fold_left (fun g (v, u) -> connect_with f v u el g) g (Seq.product vs us)

  let skeleton g =
    Vertex_map.map (fun _ (_, adj) -> ((), Edge_map.map (fun _ _ -> ()) adj)) g

  let of_edges es = 
    empty
    |> (Fun.flip @@ Seq.fold_left (fun g v -> add v () g)) (Seq.append (Seq.map fst es) (Seq.map snd es))
    |> (Fun.flip @@ Seq.fold_left (fun g (v, u) -> connect v u () g)) es

  let of_labeled_edges es = 
    empty
    |> (Fun.flip @@ Seq.fold_left (fun g v -> add v () g)) (Seq.append (Seq.map (fun (v, _, _) -> v) es) (Seq.map (fun (_, u, _) -> u) es))
    |> (Fun.flip @@ Seq.fold_left (fun g (v, u, el) -> connect v u el g)) es

  let add_segment_safe v vl adj g =
    let g = if Vertex_map.mem v g
      then g
      else add v vl g
    in
    Seq.fold_left (fun g (u, el) -> connect v u el g) g adj

  let vertex_label v g =
    let (vl, _) = Vertex_map.find v g in
    vl

  let edge_label v u g =
    let (_, adj) = Vertex_map.find v g in
    Edge_map.find u adj

  let vertex_mem v g =
    Vertex_map.mem v g

  let edge_mem v u g =
    let (_, adj) = Vertex_map.find v g in
    Edge_map.mem u adj

  let adjacent v g =
    let (_, adj) = Vertex_map.find v g in
    Edge_map.to_seq adj

  let context v g =
    let (vl, adj) = Vertex_map.find v g in
    (vl, Edge_map.to_seq adj)

  let adjacency_list g =
    Seq.map (fun (v, (vl, adj)) -> ((v, vl), Edge_map.to_seq adj)) (Vertex_map.to_seq g)

  let to_seq g =
    Seq.fold_left (fun acc (v, (vl, adj)) -> 
        (Seq.cons (v, vl) *** Seq.append (Seq.map (fun (u, el) -> (v, u, el)) (Edge_map.to_seq adj))) acc) 
      (Seq.empty, Seq.empty) (Vertex_map.to_seq g)

  let of_seq lvs les =
    let g = Seq.fold_left (fun g (v, vl) -> add v vl g) Vertex_map.empty lvs in
    Seq.fold_left (fun g (v, u, el) -> connect v u el g) g les

  let labeled_vertices g =
    Seq.map (fun (v, (vl, _)) -> (v, vl)) (Vertex_map.to_seq g)

  let labeled_vertices_map f g =
    Vertex_map.map (fun v (vl, adj) -> (f v vl, adj)) g

  let labeled_edges g =
    Vertex_map.to_seq g
    |> Seq.flat_map (fun (v, (_, adj)) -> Seq.map (fun (u, el) -> (v, u, el)) (Edge_map.to_seq adj))

  let edge_labels g =
    Vertex_map.to_seq g
    |> Seq.flat_map (fun (_, (_, adj)) -> Seq.map (fun (_, el) -> el) (Edge_map.to_seq adj))

  let labeled_edges_map f g =
    Vertex_map.map (fun v (vl, adj) -> (vl, Edge_map.map (fun u el -> f v u el) adj)) g

  let vertices g =
    Vertex_map.to_seq g
    |> Seq.map fst

  let edges g =
    Vertex_map.to_seq g
    |> Seq.flat_map (fun (v, (_, adj)) -> Seq.map (fun (u, _) -> (v, u)) (Edge_map.to_seq adj))

  let pp pp_vl pp_el ppf g =
    let pp_labeled_vertex ppf (v, vl) = Fmt.pf ppf "@[%a%a@]" Vertex.pp v pp_vl vl in
    let pp_labeled_edge ppf (v, u, el) = Fmt.pf ppf "@[%a->%a%a@]" Vertex.pp v Vertex.pp u pp_el el in
    Fmt.(parens (pair ~sep:comma (braces (seq ~sep:comma pp_labeled_vertex)) (braces (seq ~sep:comma pp_labeled_edge)))) ppf (to_seq g)

  let transpose g =
    let lvs, les = to_seq g in
    of_seq lvs (Seq.map (fun (v, u, el) -> (u, v, el)) les)

  let to_dot ~string_of_vertex_label ~string_of_edge_label g =
    let lvs, les = to_seq g in
    let nodes = Seq.map (fun (v, l) -> 
        Dot.(node (Vertex.to_id v) ~attrs:[
            "xlabel" => String (Fmt.to_to_string Vertex.pp v);
            "label" => String (string_of_vertex_label l)
          ])) 
        lvs 
    in
    let edges = Seq.map (fun (v, u, l) ->
        Dot.(edge Directed (Vertex.to_id v) (Vertex.to_id u) ~attrs:[
            "label" => String (string_of_edge_label l)
          ])) 
        les
    in
    Dot.((Digraph, "g", List.of_seq (Seq.append nodes edges)))

  let apply f g t =
    Vertex_map.map (fun v x' ->
        let (vl, adj) = Vertex_map.find v g in
        f x' vl (Seq.map (fun (u, el) -> (el, Vertex_map.find u t)) @@ Edge_map.to_seq adj))
      t

  let fold ~eq seed f g =
    let seeds = Vertex_map.map (fun v (vl, _) -> seed v vl) g in
    let t = Fixedpoint.run ~eq:(Vertex_map.equal eq) (apply f g) seeds in
    fun v -> Vertex_map.find v t

  let dijkstra seed f' g v' =
    let b = ref @@ Vertex_map.empty in
    let f = ref @@ Vertex_map.map (fun v (vl, _) -> seed v vl) g in

    let rec visit v =
      let (vl, adj) = Vertex_map.find v g in
      b := Vertex_map.add v () !b;
      let x = f' (Vertex_map.find v !f) vl (Seq.map (fun (u, el) -> (el, fun () -> 
          if not @@ Vertex_map.mem u !b
          then visit u;
          Vertex_map.find u !f))
          (Edge_map.to_seq adj))
      in
      f := Vertex_map.add v x !f
    in
    visit v';
    Vertex_map.find v' !f

  let digraph seed f' g =
    let n = ref @@ Vertex_map.map (fun _ _ -> Size.zero) g in
    let f = ref @@ Vertex_map.map (fun v (vl, _) -> seed v vl) g in
    let s = Stack.create () in

    let rec unwind v x = 
      let u = Stack.pop s in
      if Vertex.equal v u then () else begin
        n := Vertex_map.add u Size.top !n;
        f := Vertex_map.add u x !f;
        unwind v x
      end
    in
    let rec visit v =
      Stack.push v s;
      let d = Size.of_int @@ Stack.length s in
      n := Vertex_map.add v d !n;
      let (vl, adj) = Vertex_map.find v g in
      let (d', adj) =
        Seq.fold_left_map (fun d' (u, el) ->
            (Size.min d' (Vertex_map.find u !n), (el, fun () ->
                 if Size.equal (Vertex_map.find u !n) (Size.zero)
                 then visit u;
                 Vertex_map.find u !f)))
          d (Edge_map.to_seq adj)
      in
      let x = f' (Vertex_map.find v !f) vl adj in
      f := Vertex_map.add v x !f;
      if d = d' then unwind v x
    in
    Vertex_map.iter (fun v _ -> visit v) g;
    fun v -> Vertex_map.find v !f

  let unfold f v start =
    let graph = ref empty in

    let rec cycle v x = 
      if Vertex_map.mem v !graph
      then v
      else 
        let (vl, adj) = f v x in
        graph := add v vl !graph;
        let adj = Seq.map (fun (el, v, x) -> (el, cycle v x)) adj in
        Seq.iter (fun (el, u) -> graph := connect v u el !graph) adj;
        v
    in
    let _ = cycle v start in
    !graph

    (*let graph = ref empty in

    let rec cycle v x = 
      if Vertex_map.mem v !graph
      then ()
      else 
        let (vl, adj) = f !graph v x in
        graph := add v vl !graph;
        Seq.iter (fun (el, u, _) -> graph := connect v u el !graph) adj;
        Seq.iter (fun (_, u, x) -> cycle u x) adj;
    in
    cycle v start;
    !graph*)

  let rec tie f g =
    let m = lazy begin
      Vertex_map.map (fun v (vl, adj) ->
          f (tie f g) v vl (Edge_map.to_seq adj))
        g
    end in
    fun v -> Vertex_map.find v (Lazy.force m)

  let relabel pm g =
    Seq.fold_left (fun g' (v, (vl, adj)) ->
        let adj' = Edge_map.of_seq @@ Seq.map (fun (v, el) -> (pm v, el)) @@ Edge_map.to_seq adj in
        Vertex_map.add (pm v) (vl, adj') g')
      empty (Vertex_map.to_seq g)

  module type INDEX = sig
    type t
    type elt
    val update: elt -> t -> [> `New of vertex * elt * t | `Old of vertex]
    val make: vertex Supply.t -> t
  end
  module Gen(Index: INDEX) = struct

    let unfold_multiple ~supply ?(merge = Fun.const) f start =
      let graph = ref empty in
      let index = ref (Index.make supply) in

      let rec cycle x = 
        match Index.update x !index with
        | `Old v -> v
        | `New (v, x, index') ->
          index := index';

          let (vl, adj_xs) = f v x in
          graph := add v vl !graph;

          let adj = Seq.map (fun (el, x) -> (el, cycle x)) adj_xs in
          Seq.iter (fun (el, u) -> graph := connect_with merge v u el !graph) adj;
          v
      in
      let vs = List.map cycle start in
      (vs, !graph)

    let unfold ~supply ?(merge = Fun.const) f start =
      let vs, g = unfold_multiple ~supply ~merge f [start] in
      (List.the vs, g)
  end
end

(*let mem_adj adj g = List.for_all (fun (v, _) -> Vertex_map.mem v g) adj

let isomorphism (type vl) (eq: vl Equal.t) g1 g2 = (* trivial for labeled graphs *)
  let vs1 = vertices g1 in
  let vs2 = vertices g2 in
  let module Monad = ResultMonad.Make(struct type t = vl labeled_vertex end) in
  let module Traverse = List.Traverse(Monad) in

  Monad.map Array.of_list @@
  Traverse.map_m (fun (v, vl) ->
      match List.find_idx Fun.(eq vl % snd) vs2 with
      | Some (idx, _) -> Ok idx
      | None -> Error (v, vl))
    vs1

let spanning_forest ~indices g =
  let visited = ref IntSet.empty in

  let rec visit v =
    if IntSet.mem v !visited
    then None
    else begin
      visited := IntSet.add v !visited;
      let adj = get_adj v g in
      let f = List.filter_map (fun (v, _) -> visit v) adj in
      Some (Tree.NodeLabeled.node v f)
    end
  in
  List.filter_map visit indices

let rec post_order_node t =
  DifferenceList.(post_order_forest (Tree.NodeLabeled.subforest t) @ singleton (Tree.NodeLabeled.label t))

and post_order_forest f =
  List.fold_right (fun n l -> DifferenceList.(post_order_node n @ l)) f DifferenceList.empty 

let post_order g =
  let f = spanning_forest ~indices:(indices g) g in
  DifferenceList.to_list @@ post_order_forest f

let topo_sort g =
  List.rev @@ post_order g


let goto ~eq v el g = 
  let adj = get_adj v g in
  ListExt.cossa ~eq el adj

let rec path ~eq v els g =
  match els with
  | [] -> [v]
  | el :: els ->
    let v' = goto ~eq v el g in
    v :: path ~eq v' els g

let walk g = 
  List.flat_map (fun (v, els) ->
      let adj = get_adj v g in
      List.map (fun (v, el) -> (v, el :: els)) adj)

*)
