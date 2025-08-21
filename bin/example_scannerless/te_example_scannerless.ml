open! Te_bot
open Te_core
module T = Types

module X = Spec.Build(functor(Context: Spec.CONTEXT) -> struct
    open Context
    let Vector.[s'; s; a; b; c; u] = variables Vector.["S'"; "S"; "A"; "B"; "C"; "_"]
    let syntactic = [s'; s; a; b]
    let lexical = [c]
    let labels = [u]
    let longest_match = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(var a * (codes "y" + var c));
        make (u, a) (codes "x");
      ]

    let scanner =
      Production.[
      make (u, c) R.(codes "z" * codes "z");
    ]
end)

let _ =
  let d = X.driver (X.tables ()) in
  let open X.Run in
  d#read (code "x");
  d#read (code "y");
  d#read eof;

  Fmt.pr "%a" Trace.pp d#trace;

  Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot);
  Fmt.pr "@[%s@]" (Dot.string_of_graph (T.Node_packed_forest.to_dot d#forest))
