open Te_bot
open Te_core
module T = Types

module R = Re.Porcelan(Re.Abstract)

let set x = R.lits Tn.Lits.(codes (T.Codes.of_string x))

let var x = R.lits Tn.Lits.(var x)

let () = 
  let Vector.[s'; s; b] = T.Var.make ~supply:T.Var.supply Vector.["S'"; "S"; "B"] in
  let ps = Tn.Production.[
      make s' R.(var s * plus (set "$"));
      make s R.((set "a" + var b) * (star (set "c")));
      make b R.((set "d" * var b) + null); 
  ]
  in
  print_endline @@
  Dot.string_of_graph @@
  Tn.M.to_dot ~string_of_labels:(fun _ -> "") ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) (Tn.convert ~supply:T.State.supply (Tn.Production.make s R.(((set "d" + var b) + null))));
  print_endline @@
  Dot.string_of_graph @@
  Tn.M.to_dot ~string_of_labels:(fun _ -> "") ~string_of_lits:(fun x -> Fmt.to_to_string Tn.Lits.pp x) (Tn.expand ~supply:T.State.supply s' ps)

