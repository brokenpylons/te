open! Te_bot
open Te_core
module T = Types

module X = Spec.Build(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; b; c; u; op; val_] = variables Vector.["S'"; "S"; "A"; "b"; "c"; "_"; "Op"; "Val"]
    let syntactic = [s'; s; a]
    let lexical = [b; c]
    let labels = [u; op; val_]
    let longest_match = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (op, s) R.(var a * var c * var s);
        make (u, s) (var a);
        make (u, a) (var b);
      ]

    let scanner =
      Production.[
        make (val_, b) (codes "x");
        make (u, c) (codes "y");
      ]
end)

let _ =
  let d = X.driver (X.tables ()) in
  let open X.Run in
  d#read (code "x");
  d#read (code "y");
  d#read (code "x");
  d#read eof;

  Fmt.pr "%a" Trace.pp d#trace;

  Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot);
  Fmt.pr "@[%s@]" (Dot.string_of_graph (T.Node_packed_forest.to_dot d#forest))
