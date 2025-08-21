open! Te_bot
open Te_core
module T = Types

module X = Spec.Build(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; b; c; u] = variables Vector.["S'"; "S"; "A"; "b"; "c"; "_"]
    let syntactic = [s'; s; a]
    let lexical = [b; c]
    let labels = [u]
    let longest_match = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(var a + var b * star (var c * var b));
        make (u, a) R.(var b * var c);
      ]

    let scanner =
      Production.[
        make (u, b) (codes "x");
        make (u, c) (codes "y");
      ]
end)

let _ =
  let d = X.driver (X.tables ()) in
  let open X.Run in
  d#read (code "x");
  d#read (code "y");
  d#read (code "x");
  d#read (code "y");
  d#read (code "x");
  d#read eof;

  Fmt.pr "%a" Trace.pp d#trace;

  Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot);
  Fmt.pr "@[%s@]" (Dot.string_of_graph (T.Node_packed_forest.to_dot d#forest))
