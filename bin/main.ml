open Te_bot
open Te_core
module T = Types

module R = Re.Porcelan(Re.Abstract)

let set x = R.lits Tn.Lits.(codes (T.Codes.of_string x))

let var x = R.lits Tn.Lits.(var x)

let eof = R.lits Tn.Lits.eof

let () = 
  let Vector.[s'; s; a; b; c; u] = T.Var.make ~supply:T.Var.supply Vector.["S'"; "S"; "A"; "B"; "C"; "_"] in
  let ps = List.to_seq @@ Tn.Production.[
      make (u, s') R.(var s * plus eof);
      make (u, s) R.(star (var a + var b + var c));
      make (u, a) R.(plus (set "a" * set "b") * set "a");
      make (u, b) R.(set "a" * star (set "b" * set "a"));
      make (u, c) (set "a");

      (*make (u, s) R.((set "a" * var b) * (star (set "c")) + (var b * set "g"));*)
      (*make (u, b) R.((var b * set "d" ) + null);*)
  ]
  in
  print_endline @@
  Dot.string_of_graph @@
  (*Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Lhss.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) (Tn.collapse ~supply:T.State.supply @@ Tn.erase_return @@ Tn.subset ~supply:T.State.supply @@ Tn.construct ~supply:T.State.supply s' ps)*)
  (*Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Lhss.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) (Tn.earley ~supply:T.State.supply @@ Tn.construct ~supply:T.State.supply s' ps)*)
  Tn.M.to_dot ~string_of_labels:(fun x -> Fmt.to_to_string Tn.Lhss.pp x) ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) (Tn.collapse ~supply:T.State.supply @@ Tn.erase_return @@ Tn.earley ~supply:T.State.supply @@ Tn.construct ~supply:T.State.supply s' ps)
