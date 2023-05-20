open Te_bot
open! Prelude 
module T = Types
open Tn

module G = T.Vertex_graph

type t = (unit, unit) G.t

let push v u g = 
  g
  |> G.add u
  |> G.connect u v

(*
module G = T.State_graph
type t = {
  stack: (T.State.t, unit) T.State_graph.t;
  top: T.States.t;
}
[@@deriving eq, ord]

let call q tn =
  Seq.filter_map (fun (to_, ls) -> if Lits.subset ls Lits.call then Some to_ else None) (M.adjacent q tn)

let rec backtrack gss vs tn =
  if Seq.is_empty vs
  then Seq.empty
  else
    let f, n = Seq.partition (fun (v, u) ->  Lits.subset (M.lits u v tn) Lits.call) @@ Seq.flat_map (fun v -> Seq.map (fun (u, ()) -> (v, u)) (G.adjacent v gss)) vs in
    (Seq.append f (backtrack gss (Seq.map fst n) tn)) 

let reduce gss v tn =
  let fs = backtrack gss (Seq.return v) tn in
  let out = Lits.vars (T.Vars.of_seq (Seq.map Lhs.symbol (Lhss.to_seq (M.output (G.vertex_label v gss) tn)))) in
  Seq.flat_map (fun (w, _) ->
      Seq.filter_map (fun (to_, ls) -> if Lits.subset ls out then Some (w, to_) else None) (M.adjacent (G.vertex_label w gss) tn))
    fs

let shift q pos tn =
  Seq.filter_map (fun (to_, ls) -> if T.Codes.is_empty (ls.Lits.codes) then Some (ls.Lits.codes, to_) else None) (M.adjacent q tn)

module G_to = Balanced_binary_tree.Map.Size(struct 
    type nonrec t = t
    let compare = compare
end)
module Gen = G.Gen(T.State_index(G_to))

let simulate tn =
  let supply = ref T.State.supply in
  Gen.unfold (fun _ gss ->
      let gss = Fixedpoint.run ~eq:equal (fun gss ->
          Seq.fold_left (fun gss v ->
              let gss = Seq.fold_left (fun gss q ->
                  let u, supply' = Supply.get !supply in
                  supply := supply';
                  {top = T.States.add u gss.top; stack = G.connect v u () (G.add u q gss.stack)})
                  gss (call (G.vertex_label v gss.stack) tn)
              in
              Seq.fold_left (fun gss (w, q) -> 
                  let u, supply' = Supply.get !supply in
                  supply := supply';
                  {top = T.States.add u gss.top; stack = G.connect w u () (G.add u q gss.stack)})
                gss (reduce gss.stack v tn))
            gss (T.States.to_seq gss.top))
          gss
      in
      Seq.flat_map (fun v ->
          Seq.map (fun q ->
              let u, supply' = Supply.get !supply in
              supply := supply';
              {top = T.States.add u gss.top; stack = G.connect v u () (G.add u q gss.stack)})
            (shift (G.vertex_label v gss.stack)


      (T.States.to_seq gss.top)
     ((), Seq.empty))
*)


