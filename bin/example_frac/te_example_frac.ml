open! Te_bot
open Te_core
module T = Types

module X = Spec.Build(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; b; c; d; e; u] = variables Vector.["S'"; "S"; "A"; "b"; "c"; "d"; "e"; "_"]
    let syntactic = [s'; s; a]
    let lexical = [b; c; d; e]
    let labels = [u]
    let longest_match = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(var b * var c * var d + var a * var e);
        make (u, a) (var b)
      ]

    let scanner =
      Production.[
        make (u, b) (codes "x");
        make (u, c) (codes "y");
        make (u, d) (codes "z");
        make (u, e) R.(codes "y" * codes "z");
      ]
end)

let _ =
  let d = X.driver (X.tables ()) in
  let open X.Run in
  d#read (code "x");
  d#read (code "y");
  d#read (code "z");
  d#read eof;

  Fmt.pr "%a" Trace.pp d#trace;

  Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot);
  Fmt.pr "@[%s@]" (Dot.string_of_graph (T.Node_packed_forest.to_dot d#forest))
