open! Te_bot
open Te_core
module T = Types

module R = Re.Porcelan(Re.Abstract)

let set x = R.lits Tn.Lits.(codes (T.Codes.of_string x))

let var x = R.lits Tn.Lits.(var x)

let eof = R.lits Tn.Lits.eof

(*let () = Fmt.pr "%a" Tn.Rhs.pp (Tn.Rhs.simplify R.((set "a" * set "b" * set "c") * (set "e" * set "d" * set "f")))

let () =
  let Vector.[v; u] = T.Var.make ~supply:T.Var.supply Vector.["V"; "_"] in
  let (_, m) = Tn.convert ~supply:(T.State.supply) (Tn.Production.make (u, v) R.(star (set "a") * star (set "b")))
  in Fmt.pr "%a" Tn.Rhs.pp (Tn.Rhs.simplify @@ Tn.M.to_re m)*)

let () = 
  (*let Vector.[s'; s; a; b; u] = T.Var.make ~supply:T.Var.supply Vector.["S'"; "S"; "A"; "B"; "_"] in
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

      (*make (u, s) R.(var a + var b);
      make (u, a) R.(var c * set "c");
      make (u, b) R.(set "a" * set "c" * (star (var c) + var d));
      make (u, c) R.(opt (set "a"));
      make (u, d) (set "a");*)

      (*make (u, s) R.((set "a" * var b) * (star (set "c")) + (var b * set "g"));*)
      (*make (u, b) R.((var b * set "d" ) + null);*)

      make (u, s) R.(var a + var b);



      (*make (u, s) R.((var s * set "a") + var a);
      make (u, a) R.((set "b" * var a) + null);*)

      (*make (u, s) R.(set "b" * var a * set "d");
      make (u, s) R.(set "a" * var a * set "c");
      make (u, s) R.(set "a" * set "g" * set "d");
      make (u, s) R.(set "b" * set "g" * set "c");
      make (u, a) (var b);
      make (u, b) (set "g");*)

      (*make (u, a) R.(var c * set "c");
      make (u, b) R.(set "a" * set "c" * (star (var c) + var d));
      make (u, c) R.(opt (set "a"));
      make (u, d) (set "a");*)
  ]
  in*)

  let Vector.[s'; s; e; _t; _u; p; r; n] = T.Var.make ~supply:T.Var.supply Vector.["S'"; "S"; "Expr"; "Term"; "Unary"; "Primary"; "Op"; "_"] in
  let ps = List.to_seq @@ Tn.Production.[
      make (n, s') R.(var s * plus eof);

      make (n, s) (var e);
      make (n, e) R.(var e * var r * var p + var e * var r * var p + var p);

      (*make (n, t) R.(var t * var r * var u + var t * var r * var u + var u);

      make (n, u) R.(var r * var p + var p);*)

      make (n, p) (var r);

      make (n, r) R.(set "#" * (set "+" + set "-" + set "*" + set "/" + plus (set "0" + set "1" + set "2" + set "3")));
    ];
  in

  (*Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Lhss.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) (Tn.collapse ~supply:T.State.supply @@ Tn.erase_return @@ Tn.subset ~supply:T.State.supply @@ Tn.construct ~supply:T.State.supply s' ps)*)
  (*Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Lhss.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) (Tn.earley ~supply:T.State.supply @@ Tn.construct ~supply:T.State.supply s' ps)*)

  (*Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Lhss.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) (Tn.minimize ~supply:T.State.supply @@ Tn.collapse ~supply:T.State.supply @@ Tn.erase_return @@ Tn.subset ~supply:T.State.supply @@ Tn.construct ~supply:T.State.supply s' ps)*)

  Fmt.pr "%a" T.Vars.pp (Tn.is_nullable ps);

  let x' = Tn.construct ~supply:T.State.supply s' ps in
  (*let x'' = Tn.convert_multiple ~supply:T.State.supply ps in*)
  let x = Tn.collapse ~supply:T.State.supply @@ Tn.construct ~supply:T.State.supply s' ps in
  let y = Tn.subset ~supply:T.State.supply @@ x' in
  (*let to_start = Tn.to_start y in
  let w = Tn.extract_multiple ~supply:T.State.supply x'' to_start y in
  let w' = Tn.strip ~supply:T.State.supply @@ Tn.construct'' s' (Tn.M.start y) w in
  let w'' = Tn.minimize ~supply:T.State.supply @@ Tn.subset ~supply:T.State.supply @@ w' in
  let z = Tn.inter y x' in*)
  begin
  Fmt.pr "%s@." @@
  Dot.string_of_graph @@
    Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) x';
  Fmt.pr "%s@." @@
  Dot.string_of_graph @@
    Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) x;
  Fmt.pr "%s@." @@
  Dot.string_of_graph @@
    Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) y;
  (*Fmt.pr "%s@." @@
  Dot.string_of_graph @@
  T.State_pair_graph.to_dot ~string_of_vertex_label:(fun x -> Fmt.to_to_string Fmt.bool x) ~string_of_edge_label:(fun x -> Fmt.to_to_string Tn.Lits.pp x) z;
  (*Fmt.pr "%s@." @@
  Dot.string_of_graph @@
  Tn.M'.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Enhanced_lits.pp x) w';*)
  Fmt.pr "%s@." @@
  Dot.string_of_graph @@
    Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) w';
  Fmt.pr "%s@." @@
  Dot.string_of_graph @@
    Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) w'';*)
  end
  (*Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Labels.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) z*)
