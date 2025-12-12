open! Te_bot
open Te_core
module T = Types

module X = Spec.Build(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; b; c; sum'; times'; value';  u] = variables Vector.["S'"; "S"; "a"; "b"; "c"; "Sum"; "Times"; "Value"; "_"]
    let syntactic = [s'; s]
    let lexical = [a; b; c]
    let labels = [u; sum'; times'; value']
    let longest_match = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (sum', s) R.(var s * var a * var s);
        make (times', s) R.(var s * var b * var s);
        make (u, s) (var c);
      ]

    let scanner =
      Production.[
        make (u, a) (codes "x");
        make (u, b) (codes "y");
        make (value', c) (codes "z");
      ]
end)

let _ =
  let d = X.driver (X.tables ()) in
  let open X.Run in
  d#read (code "z");
  d#read (code "x");
  d#read (code "z");
  d#read (code "y");
  d#read (code "z");
  d#read eof;

  Fmt.pr "%a" Trace.pp d#trace;

  Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot);
  Fmt.pr "@[%s@]" (Dot.string_of_graph (T.Node_packed_forest.to_dot d#forest))
