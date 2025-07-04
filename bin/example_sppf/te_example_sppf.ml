open! Te_bot
open Te_core
module T = Types

module X = Spec.Build(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; b; c; d; e; u] = variables Vector.["S'"; "S"; "A"; "B"; "C"; "d"; "e"; "_"]
    let syntactic = [s'; s; a; b; c]
    let lexical = [d; e]
    let labels = [u]
    let longest_match = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(var a + var e * var b);
        make (u, a) R.(var d * var a + null);
        make (u, b) R.(var c * var c);
        make (u, c) R.null;
      ]

    let scanner =
      Production.[
        make (u, d) R.(codes "x" + null);
        make (u, e) (codes "x");
      ]
end)

let _ =
  let d = X.driver (X.tables ()) in
  let open X.Run in
  d#read (code "x");
  d#read eof;

  Fmt.pr "%a" Trace.pp d#trace;

  Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot);
  Fmt.pr "@[%s@]" (Dot.string_of_graph (T.Node_packed_forest.to_dot d#forest))
