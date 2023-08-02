open Te_bot
open! Prelude
open Te_core
module T = Types

module X = Gsglr.Make(Tables.V1.Unoptimized)
module R = Re.Porcelan(Re.Abstract)

let set x = R.lits Tn.Lits.(codes (T.Codes.of_string x))

let var x = R.lits Tn.Lits.(var x)

let eof = R.lits Tn.Lits.(eof)

(*let eof = R.lits Tn.Lits.eof*)

let c_ s = T.Symbol.Code (T.Codes.the @@ T.Codes.of_string s)

let eof_ = T.Symbol.Eof

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

  let Vector.[s'; s; a; b; c; d; n] = T.Var.make ~supply:T.Var.supply Vector.["S'"; "S"; "A"; "B"; "C"; "D"; "_"] in
  let ps = List.to_seq @@ Tn.Production.[
      make (n, s') R.(var s * plus eof);
      make (n, s) R.(((var a + var b) * plus (var c)));
      make (n, b) R.((var d * var b) + null);
    ];
  in
  let ds = List.to_seq @@ Tn.Production.[
      make (n, a) (set "a");
      make (n, c) (set "c");
      make (n, d) (set "d");
    ];
  in
  let tokens = T.Vars.of_list [a; c; d] in


  (*let Vector.[s'; s; _x; a; b; z; n; _spesh; _super] = T.Var.make ~supply:T.Var.supply Vector.["S'"; "S"; "X"; "A"; "B"; "Z"; "_"; "%"; "^"] in
  let ps = List.to_seq @@ Tn.Production.[
      make (n, s') R.(var s * (plus eof));
      make (n, s) R.(var z * var a * var z * var b);
      (*make (n, s) R.(var s * var s);
      make (spesh, s) R.(var a * var a);
      make (super, s) R.(var z * var x * var z * var s);
      make (n, x) (var a);*)
    ];
  in
  let ds = List.to_seq @@ Tn.Production.[
      make (n, a) (set "a");
      make (n, b) (set "b");
      make (n, z) (set "z");
      (*make (n, a) R.(star (set "x"));
      make (super, a) (set "x");
      make (spesh, a) R.(star (set "y"));
      make (n, z) (set "z");
      make (n, b) (set "$");*)
    ];
  in
  let tokens = T.Vars.of_list [a; b; z] in*)

  let (b, g) = Tn.Builder.make ~tokens s' ps ds in
  let t = Tables.V1.Unoptimized.make ~tokens g b in
  Fmt.pr "%a" Tables.V1.Unoptimized.pp t;

  Fmt.pr "@.@[<v>";
  let d = new X.driver t in
  d#read (c_ "a");
  d#read (c_ "c");
  d#read (c_ "c");
  d#read (c_ "c");
  d#read (eof_);
  d#read (eof_);

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



