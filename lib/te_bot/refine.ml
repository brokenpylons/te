open! Prelude

module type PARTITION = sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val pp: t Fmt.t

  val empty: t
  val is_empty: t -> bool
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
end

module type VALUES = sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool

  val empty: t
  val union: t -> t -> t
end

module Abstract(P: PARTITION)(Fun: sig
    type 'a t
    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val map: ('a -> 'b) -> 'a t -> 'b t
    val return: 'a -> 'a t
  end) = struct
  type partition = P.t
  type t = {partitions: P.t Fun.t list; considered: P.t}
  [@@deriving eq, ord]
  type parts = Novel of P.t | Disjoint of P.t | Equal of P.t | Split of P.t * P.t

  module Split = struct
    type t = {partitions: parts Fun.t list; considered: P.t}
  end

  let split_partition splitter p =
    let i = P.inter splitter p in
    if P.is_empty i then 
      Disjoint p 
    else
      let d = P.diff p i in
      if P.is_empty d
      then Equal p
      else Split (i, d)

  let empty =
    {
      partitions = [];
      considered = P.empty;
    }

  let considered t =
    t.considered

  let partitions t =
    List.to_seq t.partitions

  let split splitter t =
    let novel = P.diff splitter t.considered in
    Split.{
      partitions = 
        if P.is_empty novel
        then List.map (Fun.map (split_partition splitter)) t.partitions
        else Fun.return (Novel novel) :: List.map (Fun.map (split_partition splitter)) t.partitions;
      considered = P.union splitter t.considered;
    }
end

module Set(P: PARTITION): sig
  type t
  type partition = P.t

  val refine: partition Seq.t -> t
  val considered: t -> partition
  val partitions: t -> partition Seq.t 
end = struct
  include Abstract(P)(struct
      type 'a t = 'a
      [@@deriving eq, ord]
      let map f = f
      let return = Fun.id
    end)

  let process = 
    List.concat_map (function
        | Novel p -> [p]
        | Disjoint p -> [p]
        | Equal p -> [p]
        | Split (i, d) -> [i; d])

  let add splitter t =
    let Split.{partitions; considered} = split splitter t in
    {
      partitions = process partitions; 
      considered
    }

  let refine t =
    Seq.fold_left (Fun.flip add) empty t
end

module Map(P: PARTITION)(V: VALUES): sig
  type t
  type partition = P.t
  val compare: t -> t -> int
  val equal: t -> t -> bool

  val empty: t
  val add: partition -> V.t -> t -> t
  val union: t -> t -> t
  val find: partition -> t -> V.t

  val refine: (partition * V.t) Seq.t -> t
  val considered: t -> partition
  val partitions: t -> (partition * V.t) Seq.t 
end = struct
  include Abstract(P)(struct
      type 'a t = 'a * V.t
      [@@deriving eq, ord]
      let map f (p, vs) = (f p, vs)
      let return p = (p, V.empty)
    end)

  let processes modify =
    List.concat_map (fun (p, vs) ->
        match p with
        | Novel p -> [(p, modify vs)]
        | Disjoint p -> [(p, vs)]
        | Equal p -> [(p, modify vs)]
        | Split (i, d) -> [(i, modify vs); (d, vs)])

  let add splitter v t =
    let Split.{partitions; considered} = split splitter t in
    {
      partitions = List.sort (on P.compare fst) @@ processes (V.union v) partitions;
      considered
    }

  let union t1 t2 =
    List.fold_left (fun acc (p, v) -> add p v acc) t1 t2.partitions

  let find p t =
    List.fold_left (fun acc (p', v) ->
        if P.is_empty (P.inter p p')
        then acc
        else V.union v acc)
      V.empty t.partitions

  let refine t =
    Seq.fold_left (Fun.flip @@ uncurry add) empty t
end

module Hopcroft(P: sig include PARTITION val cardinal: t -> int end) = struct
  module Set = Abstract(P)(struct
      type 'a t = 'a
      [@@deriving eq, ord]
      let map f = f
      let return = Fun.id
    end)
  type t = {active: Set.t; inactive: Set.t}

  let process aps ips =
    let aps = List.concat_map (function
        | Set.Novel _ -> []
        | Set.Disjoint p -> [p]
        | Set.Equal p -> [p]
        | Set.Split (i, d) -> [i; d])
        aps
    in
    List.bin_map (function
        | Set.Novel p -> (None, Some p)
        | Set.Disjoint p -> (None, Some p)
        | Set.Equal p -> (None, Some p)
        | Set.Split (i, d) ->
          if P.cardinal i < P.cardinal d
          then (Some i, Some d)
          else (Some d, Some i))
      aps [] ips

  let empty = {active = Set.empty; inactive = Set.empty}

  let add splitter {active = a; inactive = i} =
    let a = Set.split splitter a in
    let i = Set.split splitter i in
    let (aps, ips) = process a.partitions i.partitions in
    {
      active = {partitions = aps; considered = a.considered}; 
      inactive = {partitions = ips; considered = i.considered};
    }

  let pivot {active = a; inactive = i} = 
    match a.partitions with
    | p :: ps ->
      Some (p, {
          active = {partitions = ps; considered = a.considered};
          inactive = {partitions = p :: i.partitions; considered = i.considered}
        })
    | [] -> None

  let considered {active = a; inactive = i} =
    P.union a.considered i.considered

  let partitions {active = a; inactive = i} =
    Seq.append (List.to_seq a.partitions) (List.to_seq i.partitions)
end
