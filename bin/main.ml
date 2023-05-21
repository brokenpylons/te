open! Te_bot
open Te_core
module T = Types

module R = Re.Porcelan(Re.Abstract)

let set x = R.lits Tn.Lits.(codes (T.Codes.of_string x))

let var x = R.lits Tn.Lits.(var x)

let eof = R.lits Tn.Lits.eof

let () = Fmt.pr "%a" Tn.Rhs.pp (Tn.Rhs.simplify R.((set "a" * set "b" * set "c") * (set "e" * set "d" * set "f")))

let () =
  let Vector.[v; u] = T.Var.make ~supply:T.Var.supply Vector.["V"; "_"] in
  let (_, m) = Tn.convert ~supply:(T.State.supply) (Tn.Production.make (u, v) R.(star (set "a") * star (set "b")))
  in Fmt.pr "%a" Tn.Rhs.pp (Tn.Rhs.simplify @@ Tn.M.to_re m)

let () = 
  let Vector.[s'; s; a; b; c; d; u] = T.Var.make ~supply:T.Var.supply Vector.["S'"; "S"; "A"; "B"; "C"; "D"; "_"] in
  let ps = List.to_seq @@ Tn.Production.[
      make (u, s') R.(var s * plus eof);
      (*make (u, s) R.((var a + var b + var c));
      make (u, a) R.((set "a" * var d * set "a"));
      make (u, b) R.((set "b" * var d * set "b"));
      make (u, c) R.((set "c" * var d));
      make (u, d) R.(plus (set "d"));*)
      (*make (u, s) R.(star (var a + var b + var c));
      make (u, a) R.(plus (set "a" * set "b") * set "a");
      make (u, b) R.(set "a" * star (set "b" * set "a"));
      make (u, c) (set "a");*)

      make (u, s) R.(star (var a) + var b);
      make (u, a) R.(var c * set "c");
      make (u, b) R.(set "a" * set "c" * (star (var c) + var d));
      make (u, c) (set "a");
      make (u, d) (set "a");

      (*make (u, s) R.((set "a" * var b) * (star (set "c")) + (var b * set "g"));*)
      (*make (u, b) R.((var b * set "d" ) + null);*)
  ]
  in
  (*Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Lhss.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) (Tn.collapse ~supply:T.State.supply @@ Tn.erase_return @@ Tn.subset ~supply:T.State.supply @@ Tn.construct ~supply:T.State.supply s' ps)*)
  (*Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Lhss.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) (Tn.earley ~supply:T.State.supply @@ Tn.construct ~supply:T.State.supply s' ps)*)

  (*Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Lhss.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) (Tn.minimize ~supply:T.State.supply @@ Tn.collapse ~supply:T.State.supply @@ Tn.erase_return @@ Tn.subset ~supply:T.State.supply @@ Tn.construct ~supply:T.State.supply s' ps)*)

  let x' = Tn.construct ~supply:T.State.supply s' ps in
  let x = Tn.collapse ~supply:T.State.supply @@ Tn.construct ~supply:T.State.supply s' ps in
  let y = Tn.minimize ~supply:T.State.supply @@ Tn.subset ~supply:T.State.supply x' in
  let z = Tn.inter y x' in
  begin
  Fmt.pr "%s" @@
  Dot.string_of_graph @@
    Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) x';
  Fmt.pr "%s" @@
  Dot.string_of_graph @@
    Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) x;
  Fmt.pr "%s" @@
  Dot.string_of_graph @@
    Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) y;
  Fmt.pr "%s" @@
  Dot.string_of_graph @@
    T.State_pair_graph.to_dot ~string_of_vertex_label:(fun x -> Fmt.to_to_string Fmt.bool x) ~string_of_edge_label:(fun x -> Fmt.to_to_string Tn.Lits.pp x) z
  end
  (*Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) z*)
