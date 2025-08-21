open! Te_bot
open Te_core
module T = Types

module X = Spec.Build(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; b; c; d; e; f; g; u] = variables Vector.["S'"; "S"; "A"; "B"; "C"; "d"; "e"; "f"; "g"; "_"]
    let syntactic = [s'; s; a; b; c]
    let lexical = [d; e; f; g]
    let labels = [u]
    let longest_match = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(var a * var e);
        make (u, a) R.(var b * var f);
        make (u, b) R.(var c * opt (var g));
        make (u, c) (var d);
      ]

    let scanner =
      Production.[
        make (u, d) (codes "x");
        make (u, e) R.(plus (codes "x"));
        make (u, f) R.(codes "x" * codes "x");
        make (u, g) (codes "x");
      ]
end)

let _ =
  let d = X.driver (X.tables ()) in
  let open X.Run in
  d#read (code "x");
  d#read (code "x");
  d#read (code "x");
  d#read (code "x");
  d#read eof;

  Fmt.pr "%a" Trace.pp d#trace;

  Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot);
  Fmt.pr "@[%s@]" (Dot.string_of_graph (T.Node_packed_forest.to_dot d#forest))
