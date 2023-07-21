open Te_bot
open! Prelude
open Te_core
module T = Types

module X = Gsglr.Make(Tables.V1.Unoptimized)
module R = Re.Porcelan(Re.Abstract)

let set x = R.lits Tn.Lits.(codes (T.Codes.of_string x))

let var x = R.lits Tn.Lits.(var x)

(*let eof = R.lits Tn.Lits.eof*)

let c s = Tables.V1.Unoptimized.Code (T.Codes.the @@ T.Codes.of_string s)

let _ =
  (*let Vector.[s'; s; a; b; x; d; n] = T.Var.make ~supply:T.Var.supply Vector.["S'"; "S"; "A"; "B"; "x"; "$"; "_"] in
  let ps = List.to_seq @@ Tn.Production.[
      make (n, s') R.(var s * (var b) * (var b));
      make (n, s) R.(var s * var s + var a * var a);
      make (n, a) R.(star (var x));
      make (n, b) (var d);
      make (n, x) (set "x");
      make (n, d) (set "$");
    ];*)
  let Vector.[s'; s; a; b; n; spesh; super] = T.Var.make ~supply:T.Var.supply Vector.["S'"; "S"; "A"; "B"; "_"; "%"; "^"] in
  let ps = List.to_seq @@ Tn.Production.[
      make (n, s') R.(var s * (plus (var b)));
      make (n, s) R.(var s * var s);
      make (spesh, s) R.(var a * var a)
    ];
  in
  let ds = List.to_seq @@ Tn.Production.[
      make (n, a) R.(star (set "x"));
      make (super, a) (set "x");
      make (spesh, a) R.(star (set "y"));
      make (n, b) (set "$");
    ];
  in
  let tokens = T.Vars.of_list [a; b] in

  let (b, g) = Tn.Builder.make ~tokens s' ps ds in
  let t = Tables.V1.Unoptimized.make ~tokens g b in
  Fmt.pr "%a" Tables.V1.Unoptimized.pp t;

  Fmt.pr "@.@[<v>";
  let d = new X.driver t in
  d#read (c "x");
  d#read (c "x");
  d#read (c "y");
  d#read (c "y");
  d#read (c "$");

  (*d#load (v a);*)
  (*d#read (c "y");
  d#read (c "y");*)

  (*d#read (c "$");
  d#read (c "$");
  d#read (c "$");*)
  Fmt.pr "@]";

  (*d#clear;
  d#read' (c "$");
  d#read (c "$");

  d#clear;
  d#read' (c "$");
  d#read (c "$");

  d#clear;
  d#read' (c "$");
  d#read (c "$");

  d#clear;
  d#read' (c "$");
  d#read (c "$");*)

  (*d#read (c "y");
  d#read (c "x");
  d#read (c "x");
  d#read (c "x");*)
  Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot);
  Fmt.pr "@[%s@]" (Dot.string_of_graph (T.Node_packed_forest.to_dot d#forest))



