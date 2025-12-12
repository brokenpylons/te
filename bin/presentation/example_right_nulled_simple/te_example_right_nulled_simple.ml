open! Te_bot
open Te_core
module T = Types

module X = Spec.Build(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; b; u] = variables Vector.["S'"; "S"; "B";  "_"]
    let syntactic = [s'; s; b]
    let lexical = []
    let labels = [u]
    let longest_match = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(codes "a" * var b);
        make (u, b) R.null;
      ]

    let scanner = []
end)

let _ =
  let d = X.driver (X.tables ()) in
  let open X.Run in
  d#read (code "a");
  d#read eof;

  Fmt.pr "%a" Trace.pp d#trace;

  Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot);
  Fmt.pr "@[%s@]" (Dot.string_of_graph (T.Node_packed_forest.to_dot d#forest))
