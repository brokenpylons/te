open Te_bot
open Te_core
module T = Types


module New = Spec.Build(functor(Context: Spec.CONTEXT) -> struct
  open Context

  let Vector.[s'; s; a; b; c; d; l0; l1; u] = variables Vector.["S'"; "S"; "A"; "B"; "C"; "d"; "l0"; "l1"; "_"]
  let syntactic = [s'; s; a; b; c]
  let lexical = [d; l0; l1]

  let start = s'

  let parser =
    Production.[
      make (u, s') R.(var s * plus eof);
      make (u, s) R.(var a + var d * var d);
      make (u, a) R.(var b * var l0 + var c * var l1);
      make (u, b) (var d);
      make (u, c) (var d);
    ]

  let scanner =
    Production.[
      make (u, d) (codes "x");
      make (u, l0) R.(plus (codes "x") * codes "a");
      make (u, l1) R.(plus (codes "x") * codes "b");
    ]
end)

let _ =
  New.driver ()
