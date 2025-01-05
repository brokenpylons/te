open Te_bot
open Te_core
module T = Types

(*
  nondeterministic NC scan connections cannot be used because the NC shifts are performed only once from a node.
*)


let extra_info d =
  Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot);
  Fmt.pr "@[%s@]" (Dot.string_of_graph (Types.Node_packed_forest.to_dot d#forest))

let test_case driver Spec.{name; input; trace} =
  Alcotest.test_case name `Quick (fun () ->
      let d = driver () in
      List.iter d#read input;
      extra_info d;
      Alcotest.check (module Trace) "trace" trace d#trace)

let test_cases driver tests =
  List.map (test_case driver) tests

module Noop = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; u] = variables Vector.["S'"; "_"]
    let syntactic = [s']
    let lexical = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(plus eof);
      ]

    let scanner = []

    let tests =
      [
        Test.{
          name = "base";
          input = [eof];
          trace = Trace.[
              load eof (vertex 0 0) (vertex 1 1)
            ]
        }
      ]
  end)

module Null = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context
    let Vector.[s'; u] = variables Vector.["S'"; "_"]
    let syntactic = [s']
    let lexical = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(null * plus eof);
      ]

    let scanner = []

    let tests =
      [
        Test.{
          name = "base";
          input = [eof];
          trace = Trace.[
              load eof (vertex 0 0) (vertex 1 1)
            ]
        }
      ]
  end)

module Load = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; u] = variables Vector.["S'"; "_"]
    let syntactic = [s']
    let lexical = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(codes "a" * plus eof);
      ]

    let scanner = []

    let tests = [
      Test.{
        name = "base";
        input = [
          code "a";
          eof
        ];
        trace = Trace.[
            load (code "a") (vertex 0 0) (vertex 1 1);
            load eof (vertex 1 1) (vertex 2 2);
          ]
      }
    ]
  end)

module Load2 = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; u] = variables Vector.["S'"; "_"]
    let syntactic = [s']
    let lexical = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(codes "a" * codes "b" * plus eof);
      ]

    let scanner = []

    let tests = [
      Test.{
        name = "base";
        input = [
          code "a";
          code "b";
          eof
        ];
        trace = Trace.[
            load (code "a") (vertex 0 0) (vertex 1 1);
            load (code "b") (vertex 1 1) (vertex 2 2);
            load eof (vertex 2 2) (vertex 3 3);
          ]
      }
    ]
  end)

module Null_reduce = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; u] = variables Vector.["S'"; "S"; "_"]
    let syntactic = [s'; s]
    let lexical = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.null;
      ]

    let scanner = []

    let tests = [
      Test.{
        name = "base";
        input = [
          eof
        ];
        trace = Trace.[
            reduce (u, s) (vertex 0 0) (vertex 0 0) (vertex 1 0);
            load eof (vertex 1 0) (vertex 2 1);
          ]
      }
    ]
  end)

(* REGRESSION the shift null actions were excluded in table construction *)
module Null_shift = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; a; u] = variables Vector.["S'"; "a"; "_"]
    let syntactic = [s']
    let lexical = [a]

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var a * plus eof);
      ]

    let scanner =
      Production.[
        make (u, a) R.null;
      ]

    let tests = [
      Test.{
        name = "base";
        input = [
          eof
        ];
        trace = Trace.[
            order (vertex 0 0) a;
            expand (vertex 0 0) (vertex 3 0);
            reduce (u, a) (vertex 0 0) (vertex 0 0) (vertex 1 0);
            load eof (vertex 1 0) (vertex 2 1);
          ]
      }
    ]
  end)

module Load_reduce = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; u] = variables Vector.["S'"; "S"; "_"]
    let syntactic = [s'; s]
    let lexical = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) (codes "a");
      ]

    let scanner = []

    let tests = [
      Test.{
        name = "base";
        input = [
          code "a";
          eof
        ];
        trace = Trace.[
            load (code "a") (vertex 0 0) (vertex 1 1);
            reduce (u, s) (vertex 1 1) (vertex 0 0) (vertex 2 1);
            load eof (vertex 2 1) (vertex 3 2);
          ]
      }
    ]
  end)

module Multilabel_load_reduce = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; u; p] = variables Vector.["S'"; "S"; "_"; "%"]
    let syntactic = [s'; s]
    let lexical = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) (codes "a");
        make (p, s) (codes "a");
      ]

    let scanner = []

    let tests = [
      Test.{
        name = "base";
        input = [
          code "a";
          eof
        ];
        trace = Trace.[
            load (code "a") (vertex 0 0) (vertex 1 1);
            reduce (u, s) (vertex 1 1) (vertex 0 0) (vertex 2 1);
            load eof (vertex 2 1) (vertex 3 2);
            reduce (p, s) (vertex 1 1) (vertex 0 0) (vertex 2 1);
          ]
      }
    ]

  end)

module Shift = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; a; u] = variables Vector.["S'"; "a"; "_"]
    let syntactic = [s']
    let lexical = [a]

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var a * plus eof);
      ]

    let scanner =
      Production.[
        make (u, a) (codes "a");
      ]

    let tests = [
      Test.{
        name = "base";
        input = [
          code "a";
          eof
        ];
        trace = Trace.[
            order (vertex 0 0) a;
            expand (vertex 0 0) (vertex 3 0) ;
            read (code "a") (vertex 0 0) (vertex 3 0) (vertex 4 1);
            predict (vertex 4 1) (vertex 0 0) [a];
            shift (u, a) (vertex 4 1) (vertex 0 0) (vertex 0 0) (vertex 1 1);
            load eof (vertex 1 1) (vertex 2 2);
          ]
      }
    ]
  end)

module Shift_reduce = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; u] = variables Vector.["S'"; "S"; "a"; "_"]
    let syntactic = [s'; s]
    let lexical = [a]

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) (var a);
      ]

    let scanner =
      Production.[
        make (u, a) (codes "a");
      ]

    let tests = [
      Test.{
        name = "base";
        input = [
          code "a";
          eof
        ];
        trace = Trace.[
            order (vertex 0 0) a;
            expand (vertex 0 0) (vertex 4 0) ;
            read (code "a") (vertex 0 0) (vertex 4 0) (vertex 5 1);
            predict (vertex 5 1) (vertex 0 0)  [a];
            shift (u, a) (vertex 5 1) (vertex 0 0) (vertex 0 0) (vertex 3 1);
            reduce (u, s) (vertex 3 1) (vertex 0 0) (vertex 1 1);
            load eof (vertex 1 1) (vertex 2 2);
          ]
      }
    ]
  end)

module Shift_reduce2 = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; p; a; u] = variables Vector.["S'"; "S"; "P"; "a"; "_"]
    let syntactic = [s'; s; p]
    let lexical = [a]

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) (var p);
        make (u, p) (var a);
      ]

    let scanner =
      Production.[
        make (u, a) (codes "a");
      ]

    let tests = [
      Test.{
        name = "base";
        input = [
          code "a";
          eof
        ];
        trace = Trace.[
            order (vertex 0 0) a;
            expand (vertex 0 0) (vertex 5 0) ;
            read (code "a") (vertex 0 0) (vertex 5 0) (vertex 6 1);
            predict (vertex 6 1) (vertex 0 0) [a];
            shift (u, a) (vertex 6 1) (vertex 0 0) (vertex 0 0) (vertex 4 1);
            reduce (u, p) (vertex 4 1) (vertex 0 0) (vertex 3 1);
            reduce (u, s) (vertex 3 1) (vertex 0 0) (vertex 1 1);
            load eof (vertex 1 1) (vertex 2 2);
          ]
      }
    ]
  end)

module Multilabel_shift_reduce = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; u; p] = variables Vector.["S'"; "S"; "a"; "_"; "%"]
    let syntactic = [s'; s]
    let lexical = [a]

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) (var a);
      ]

    let scanner =
      Production.[
        make (u, a) (codes "a");
        make (p, a) (codes "a");
      ]

    let tests = [
      Test.{
        name = "base";
        input = [
          code "a";
          eof
        ];
        trace = Trace.[
            order (vertex 0 0) a;
            expand (vertex 0 0) (vertex 4 0) ;
            read (code "a") (vertex 0 0) (vertex 4 0) (vertex 5 1);
            predict (vertex 5 1) (vertex 0 0) [a];
            shift (u, a) (vertex 5 1) (vertex 0 0) (vertex 0 0) (vertex 3 1);
            shift (p, a) (vertex 5 1) (vertex 0 0) (vertex 0 0) (vertex 3 1);
            reduce (u, s) (vertex 3 1) (vertex 0 0) (vertex 1 1);
            load eof (vertex 1 1) (vertex 2 2);
          ]
      }
    ]
  end)

(* DISCOVERED reduce for A was applied twice, since the edge at the begining of the path has two nodes linked to it. *)
module Same_label_reduce = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; b; u] = variables Vector.["S'"; "S"; "A"; "B"; "_"]
    let syntactic = [s'; s; a]
    let lexical = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) (var a);
        make (u, s) (var b);
        make (u, a) (codes "a");
        make (u, b) (codes "a");
      ]

    let scanner = []

    let tests = [
      Test.{
        name = "base";
        input = [
          code "a";
          eof
        ];
        trace = Trace.[
            load (code "a") (vertex 0 0) (vertex 1 1);
            reduce (u, a) (vertex 1 1) (vertex 0 0) (vertex 4 1);
            reduce (u, s) (vertex 4 1) (vertex 0 0) (vertex 2 1);
            load eof (vertex 2 1) (vertex 3 2);
            reduce (u, b) (vertex 1 1) (vertex 0 0) (vertex 4 1);
            reduce (u, s) (vertex 4 1) (vertex 0 0) (vertex 2 1);
          ]
      };
    ]
  end)

module Right_nulled = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; u] = variables Vector.["S'"; "S"; "a"; "_"]
    let syntactic = [s'; s; a]
    let lexical = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(codes "a" *  var a);
        make (u, a) R.null;
      ]

    let scanner = []

    let tests = [
      Test.{
        name = "base";
        input = [
          code "a";
          eof
        ];
        trace = Trace.[
            load (code "a") (vertex 0 0) (vertex 1 1);
            reduce (u, s) (vertex 1 1) (vertex 0 0) (vertex 3 1);
            load eof (vertex 3 1) (vertex 4 2);
            reduce (u, a) (vertex 1 1) (vertex 1 1) (vertex 2 1);
          ]
      };
    ]
  end)

module Right_nulled2 = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; u] = variables Vector.["S'"; "S"; "a"; "_"]
    let syntactic = [s'; s; a]
    let lexical = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(star (codes "x") * var a);
        make (u, a) R.null;
      ]

    let scanner = []

    let tests = [
      Test.{
        name = "n0";
        input = [
          eof
        ];
        trace = Trace.[
            reduce (u, s) (vertex 0 0) (vertex 0 0) (vertex 3 0);
            load eof (vertex 3 0) (vertex 4 1);
            reduce (u, a) (vertex 0 0) (vertex 0 0) (vertex 5 0);
          ]
      };
      Test.{
        name = "n1";
        input = [
          code "x";
          eof
        ];
        trace = Trace.[
            load (code "x") (vertex 0 0) (vertex 1 1);
            reduce (u, s) (vertex 1 1) (vertex 0 0) (vertex 3 1);
            load eof (vertex 3 1) (vertex 4 2);
            reduce (u, a) (vertex 1 1) (vertex 1 1) (vertex 2 1);
          ]
      };
    ]
  end)

module Repeat_load = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; u] = variables Vector.["S'"; "S"; "_"]
    let syntactic = [s'; s]
    let lexical = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(star (codes "a"));
      ]

    let scanner = []

    let tests = [
      Test.{
        name = "n0";
        input = [
          eof
        ];
        trace = Trace.[
            reduce (u, s) (vertex 0 0) (vertex 0 0) (vertex 2 0);
            load eof (vertex 2 0) (vertex 3 1);
          ]
      };
      Test.{
        name = "n1";
        input = [
          code "a";
          eof
        ];
        trace = Trace.[
            load (code "a") (vertex 0 0) (vertex 1 1);
            reduce (u, s) (vertex 1 1) (vertex 0 0) (vertex 2 1);
            load eof (vertex 2 1) (vertex 3 2);
          ]
      };
      Test.{
        name = "n2";
        input = [
          code "a";
          code "a";
          eof
        ];
        trace = Trace.[
            load (code "a") (vertex 0 0) (vertex 1 1);
            load (code "a") (vertex 1 1) (vertex 1 2);
            reduce (u, s) (vertex 1 2) (vertex 0 0) (vertex 2 2);
            load eof (vertex 2 2) (vertex 3 3);
          ]
      };
    ]
  end)

module Left_recursion_load = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; u] = variables Vector.["S'"; "S"; "_"]
    let syntactic = [s'; s]
    let lexical = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(var s * codes "a" + null);
      ]

    let scanner = []

    let tests = [
      Test.{
        name = "n0";
        input = [
          eof
        ];
        trace = Trace.[
            reduce (u, s) (vertex 0 0) (vertex 0 0) (vertex 1 0);
            load eof (vertex 1 0) (vertex 3 1);
          ]
      };
      Test.{
        name = "n1";
        input = [
          code "a";
          eof
        ];
        trace = Trace.[
            reduce (u, s) (vertex 0 0) (vertex 0 0) (vertex 1 0);
            load (code "a") (vertex 1 0) (vertex 2 1);
            reduce (u, s) (vertex 2 1) (vertex 0 0) (vertex 1 1);
            load eof (vertex 1 1) (vertex 3 2);
          ]
      };
      Test.{
        name = "n2";
        input = [
          code "a";
          code "a";
          eof
        ];
        trace = Trace.[
            reduce (u, s) (vertex 0 0) (vertex 0 0) (vertex 1 0);
            load (code "a") (vertex 1 0) (vertex 2 1);
            reduce (u, s) (vertex 2 1) (vertex 0 0) (vertex 1 1);
            load (code "a") (vertex 1 1) (vertex 2 2);
            reduce (u, s) (vertex 2 2) (vertex 0 0) (vertex 1 2);
            load eof (vertex 1 2) (vertex 3 3);
          ]
      };
      Test.{
        name = "n3";
        input = [
          code "a";
          code "a";
          code "a";
          eof
        ];
        trace = Trace.[
            reduce (u, s) (vertex 0 0) (vertex 0 0) (vertex 1 0);
            load (code "a") (vertex 1 0) (vertex 2 1);
            reduce (u, s) (vertex 2 1) (vertex 0 0) (vertex 1 1);
            load (code "a") (vertex 1 1) (vertex 2 2);
            reduce (u, s) (vertex 2 2) (vertex 0 0) (vertex 1 2);
            load (code "a") (vertex 1 2) (vertex 2 3);
            reduce (u, s) (vertex 2 3) (vertex 0 0) (vertex 1 3);
            load eof (vertex 1 3) (vertex 3 4);
          ]
      };
    ]
  end)

module Right_recursion_load = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; u] = variables Vector.["S'"; "S"; "_"]
    let syntactic = [s'; s]
    let lexical = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(codes "a" * var s + null);
      ]

    let scanner = []

    let tests = [
      Test.{
        name = "n0";
        input = [
          eof
        ];
        trace = Trace.[
            reduce (u, s) (vertex 0 0) (vertex 0 0) (vertex 3 0);
            load eof (vertex 3 0) (vertex 4 1);
          ]
      };
      Test.{
        name = "n1";
        input = [
          code "a";
          eof
        ];
        trace = Trace.[
            load (code "a") (vertex 0 0) (vertex 1 1);
            reduce (u, s) (vertex 1 1) (vertex 0 0) (vertex 3 1);
            load eof (vertex 3 1) (vertex 4 2);
            reduce (u, s) (vertex 1 1) (vertex 1 1) (vertex 2 1);
          ]
      };
      Test.{
        name = "n2";
        input = [
          code "a";
          code "a";
          eof
        ];
        trace = Trace.[
            load (code "a") (vertex 0 0) (vertex 1 1);
            load (code "a") (vertex 1 1) (vertex 1 2);
            reduce (u, s) (vertex 1 2) (vertex 1 1) (vertex 2 2);
            reduce (u, s) (vertex 2 2) (vertex 0 0) (vertex 3 2);
            load eof (vertex 3 2) (vertex 4 3);
            reduce (u, s) (vertex 1 2) (vertex 1 2) (vertex 2 2);
          ]
      };
      Test.{
        name = "n3";
        input = [
          code "a";
          code "a";
          code "a";
          eof
        ];
        trace = Trace.[
            load (code "a") (vertex 0 0) (vertex 1 1);
            load (code "a") (vertex 1 1) (vertex 1 2);
            load (code "a") (vertex 1 2) (vertex 1 3);
            reduce (u, s) (vertex 1 3) (vertex 1 2) (vertex 2 3);
            reduce (u, s) (vertex 2 3) (vertex 1 1) (vertex 2 3);
            reduce (u, s) (vertex 2 3) (vertex 0 0) (vertex 3 3);
            load eof (vertex 3 3) (vertex 4 4);
            reduce (u, s) (vertex 1 3) (vertex 1 3) (vertex 2 3);
          ]
      };
    ]
  end)

module Nest_load = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; u] = variables Vector.["S'"; "S"; "_"]
    let syntactic = [s'; s]
    let lexical = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(codes "a" * var s * codes "b" + null);
      ]

    let scanner = []

    let tests = [
      Test.{
        name = "n0";
        input = [
          eof
        ];
        trace = Trace.[
            reduce (u, s) (vertex 0 0) (vertex 0 0) (vertex 4 0);
            load eof (vertex 4 0) (vertex 5 1);
          ]
      };
      Test.{
        name = "n2";
        input = [
          code "a";
          code "b";
          eof
        ];
        trace = Trace.[
            load (code "a") (vertex 0 0) (vertex 1 1);
            reduce (u, s) (vertex 1 1) (vertex 1 1) (vertex 2 1);
            load (code "b") (vertex 2 1) (vertex 3 2);
            reduce (u, s) (vertex 3 2) (vertex 0 0) (vertex 4 2);
            load eof (vertex 4 2) (vertex 5 3);
          ]
      };
      Test.{
        name = "n4";
        input = [
          code "a";
          code "a";
          code "b";
          code "b";
          eof
        ];
        trace = Trace.[
            load (code "a") (vertex 0 0) (vertex 1 1);
            load (code "a") (vertex 1 1) (vertex 1 2);
            reduce (u, s) (vertex 1 2) (vertex 1 2) (vertex 2 2);
            load (code "b") (vertex 2 2) (vertex 3 3);
            reduce (u, s) (vertex 3 3) (vertex 1 1) (vertex 2 3);
            load (code "b") (vertex 2 3) (vertex 3 4);
            reduce (u, s) (vertex 3 4) (vertex 0 0) (vertex 4 4);
            load eof (vertex 4 4) (vertex 5 5);
          ]
      };
    ]
  end)

module Middle = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; u] = variables Vector.["S'"; "S"; "_"]
    let syntactic = [s'; s]
    let lexical = []

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(codes "a" * var s * codes "a" + null);
      ]

    let scanner = []

    let tests = [
      Test.{
        name = "n0";
        input = [
          eof
        ];
        trace = Trace.[
            reduce (u, s) (vertex 0 0) (vertex 0 0) (vertex 4 0);
            load eof (vertex 4 0) (vertex 5 1);
          ]
      };
      Test.{
        name = "n2";
        input = [
          code "a";
          code "a";
          eof
        ];
        trace = Trace.[
            load (code "a") (vertex 0 0) (vertex 1 1);
            reduce (u, s) (vertex 1 1) (vertex 1 1) (vertex 2 1);
            load (code "a") (vertex 2 1) (vertex 3 2);
            load (code "a") (vertex 1 1) (vertex 1 2);
            reduce (u, s) (vertex 3 2) (vertex 0 0) (vertex 4 2);
            load eof (vertex 4 2) (vertex 5 3);
          ]
      };
      Test.{
        name = "n4";
        input = [
          code "a";
          code "a";
          code "a";
          code "a";
          eof
        ];
        trace = Trace.[
            load (code "a") (vertex 0 0) (vertex 1 1);
            reduce (u, s) (vertex 1 1) (vertex 1 1) (vertex 2 1);
            load (code "a") (vertex 2 1) (vertex 3 2);
            load (code "a") (vertex 1 1) (vertex 1 2);
            reduce (u, s) (vertex 3 2) (vertex 0 0) (vertex 4 2);
            reduce (u, s) (vertex 1 2) (vertex 1 2) (vertex 2 2);
            load (code "a") (vertex 2 2) (vertex 3 3);
            load (code "a") (vertex 1 2) (vertex 1 3);
            reduce (u, s) (vertex 3 3) (vertex 1 1) (vertex 2 3);
            load (code "a") (vertex 2 3) (vertex 3 4);
            reduce (u, s) (vertex 1 3) (vertex 1 3) (vertex 2 3);
            load (code "a") (vertex 1 3) (vertex 1 4);
            reduce (u, s) (vertex 3 4) (vertex 0 0) (vertex 4 4);
            load eof (vertex 4 4) (vertex 5 5);
            reduce (u, s) (vertex 3 4) (vertex 1 2) (vertex 2 4);
          ]
      };
    ]
  end)

module Left_recursion_shift = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; u] = variables Vector.["S'"; "S"; "a"; "_"]
    let syntactic = [s'; s]
    let lexical = [a]

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(var s * var a + null);
      ]

    let scanner =
      Production.[
        make (u, a) (codes "a");
      ]

    let tests = [
      Test.{
        name = "n0";
        input = [
          eof
        ];
        trace = Trace.[
            expand (vertex 0 0) (vertex 3 0);
            reduce (u, s) (vertex 0 0) (vertex 0 0) (vertex 1 0);
            order (vertex 1 0) a;
            load eof (vertex 1 0) (vertex 5 1);
          ]
      };
      Test.{
        name = "n1";
        input = [
          code "a";
          eof
        ];
        trace = Trace.[
            expand (vertex 0 0) (vertex 3 0);
            read (code "a") (vertex 0 0) (vertex 3 0) (vertex 4 1);
            predict (vertex 4 1) (vertex 0 0) [a];
            reduce (u, s) (vertex 0 0) (vertex 0 0) (vertex 1 0);
            order (vertex 1 0) a;
            shift (u, a) (vertex 4 1) (vertex 0 0) (vertex 1 0) (vertex 2 1);
            expand (vertex 2 1) (vertex 3 1);
            reduce (u, s) (vertex 2 1) (vertex 0 0) (vertex 1 1);
            order (vertex 1 1) a;
            load eof (vertex 1 1) (vertex 5 2);
          ]
      };
      Test.{
        name = "n2";
        input = [
          code "a";
          code "a";
          eof
        ];
        trace = Trace.[
            expand (vertex 0 0) (vertex 3 0);
            read (code "a") (vertex 0 0) (vertex 3 0) (vertex 4 1);
            predict (vertex 4 1) (vertex 0 0) [a];
            reduce (u, s) (vertex 0 0) (vertex 0 0) (vertex 1 0);
            order (vertex 1 0) a;
            shift (u, a) (vertex 4 1) (vertex 0 0) (vertex 1 0) (vertex 2 1);
            expand (vertex 2 1) (vertex 3 1);
            read (code "a") (vertex 2 1) (vertex 3 1) (vertex 4 2);
            predict (vertex 4 2) (vertex 2 1) [a];
            reduce (u, s) (vertex 2 1) (vertex 0 0) (vertex 1 1);
            order (vertex 1 1) a;
            shift (u, a) (vertex 4 2) (vertex 2 1) (vertex 1 1) (vertex 2 2);
            expand (vertex 2 2) (vertex 3 2);
            reduce (u, s) (vertex 2 2) (vertex 0 0) (vertex 1 2);
            order (vertex 1 2) a;
            load eof (vertex 1 2) (vertex 5 3);
          ]
      };
      Test.{
        name = "n3";
        input = [
          code "a";
          code "a";
          code "a";
          eof
        ];
        trace = Trace.[
            expand (vertex 0 0) (vertex 3 0);
            read (code "a") (vertex 0 0) (vertex 3 0) (vertex 4 1);
            predict (vertex 4 1) (vertex 0 0) [a];
            reduce (u, s) (vertex 0 0) (vertex 0 0) (vertex 1 0);
            order (vertex 1 0) a;
            shift (u, a) (vertex 4 1) (vertex 0 0) (vertex 1 0) (vertex 2 1);
            expand (vertex 2 1) (vertex 3 1);
            read (code "a") (vertex 2 1) (vertex 3 1) (vertex 4 2);
            predict (vertex 4 2) (vertex 2 1) [a];
            reduce (u, s) (vertex 2 1) (vertex 0 0) (vertex 1 1);
            order (vertex 1 1) a;
            shift (u, a) (vertex 4 2) (vertex 2 1) (vertex 1 1) (vertex 2 2);
            expand (vertex 2 2) (vertex 3 2);
            read (code "a") (vertex 2 2) (vertex 3 2) (vertex 4 3);
            predict (vertex 4 3) (vertex 2 2) [a];
            reduce (u, s) (vertex 2 2) (vertex 0 0) (vertex 1 2);
            order (vertex 1 2) a;
            shift (u, a) (vertex 4 3) (vertex 2 2) (vertex 1 2) (vertex 2 3);
            expand (vertex 2 3) (vertex 3 3);
            reduce (u, s) (vertex 2 3) (vertex 0 0) (vertex 1 3);
            order (vertex 1 3) a;
            load eof (vertex 1 3) (vertex 5 4);
          ]
      };
    ]
  end)

module Right_recursion_shift = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; u] = variables Vector.["S'"; "S"; "a"; "_"]
    let syntactic = [s'; s]
    let lexical = [a]

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(var a * var s + null);
      ]

    let scanner =
      Production.[
        make (u, a) (codes "a");
      ]

    let tests = [
      Test.{
        name = "n0";
        input = [
          eof
        ];
        trace = Trace.[
            order (vertex 0 0) a;
            expand (vertex 0 0) (vertex 5 0);
            reduce (u, s) (vertex 0 0) (vertex 0 0) (vertex 1 0);
            load eof (vertex 1 0) (vertex 2 1);
          ]
      };
      Test.{
        name = "n1";
        input = [
          code "a";
          eof
        ];
        trace = Trace.[
            order (vertex 0 0) a;
            expand (vertex 0 0) (vertex 5 0);
            read (code "a") (vertex 0 0) (vertex 5 0) (vertex 6 1);
            predict (vertex 6 1) (vertex 0 0) [a];
            shift (u, a) (vertex 6 1) (vertex 0 0) (vertex 0 0) (vertex 3 1);
            order (vertex 3 1) a;
            expand (vertex 3 1) (vertex 5 1);
            reduce (u, s) (vertex 3 1) (vertex 0 0) (vertex 1 1);
            load eof (vertex 1 1) (vertex 2 2);
            reduce (u, s) (vertex 3 1) (vertex 3 1) (vertex 4 1);
          ]
      };
      Test.{
        name = "n2";
        input = [
          code "a";
          code "a";
          eof
        ];
        trace = Trace.[
            order (vertex 0 0) a;
            expand (vertex 0 0) (vertex 5 0);
            read (code "a") (vertex 0 0) (vertex 5 0) (vertex 6 1);
            predict (vertex 6 1) (vertex 0 0) [a];
            shift (u, a) (vertex 6 1) (vertex 0 0) (vertex 0 0) (vertex 3 1);
            order (vertex 3 1) a;
            expand (vertex 3 1) (vertex 5 1);
            read (code "a") (vertex 3 1) (vertex 5 1) (vertex 6 2);
            predict (vertex 6 2) (vertex 3 1) [a];
            shift (u, a) (vertex 6 2) (vertex 3 1) (vertex 3 1) (vertex 3 2);
            order (vertex 3 2) a;
            expand (vertex 3 2) (vertex 5 2);
            reduce (u, s) (vertex 3 2) (vertex 3 1) (vertex 4 2);
            reduce (u, s) (vertex 4 2) (vertex 0 0) (vertex 1 2);
            load eof (vertex 1 2) (vertex 2 3);
            reduce (u, s) (vertex 3 2) (vertex 3 2) (vertex 4 2);
          ]
      };
    ]
  end)

module Nest_shift = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; b; u] = variables Vector.["S'"; "S"; "a"; "b"; "_"]
    let syntactic = [s'; s]
    let lexical = [a; b]

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(var a * var s * var b + null);
      ]

    let scanner =
      Production.[
        make (u, a) (codes "a");
        make (u, b) (codes "b");
      ]

    let tests = [
      Test.{
        name = "n0";
        input = [
          eof
        ];
        trace = Trace.[
            order (vertex 0 0) a;
            expand (vertex 0 0) (vertex 8 0);
            reduce (u, s) (vertex 0 0) (vertex 0 0) (vertex 1 0);
            load eof (vertex 1 0) (vertex 2 1);
          ]
      };
      Test.{
        name = "n2";
        input = [
          code "a";
          code "b";
          eof
        ];
        trace = Trace.[
            order (vertex 0 0) a;
            expand (vertex 0 0) (vertex 8 0);
            read (code "a") (vertex 0 0) (vertex 8 0) (vertex 9 1);
            predict (vertex 9 1) (vertex 0 0) [a];
            shift (u, a) (vertex 9 1) (vertex 0 0) (vertex 0 0) (vertex 3 1);
            order (vertex 3 1) a;
            expand (vertex 3 1) (vertex 10 1);
            read (code "b") (vertex 3 1) (vertex 10 1) (vertex 7 2);
            predict (vertex 7 2) (vertex 3 1) [b];
            reduce (u, s) (vertex 3 1) (vertex 3 1) (vertex 4 1);
            order (vertex 4 1) b;
            shift (u, b) (vertex 7 2) (vertex 3 1) (vertex 4 1) (vertex 5 2);
            expand (vertex 5 2) (vertex 6 2);
            reduce (u, s) (vertex 5 2) (vertex 0 0) (vertex 1 2);
            load eof (vertex 1 2) (vertex 2 3);
          ]
      }
    ]
  end)

module Predict = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; b; c; u] = variables Vector.["S'"; "S"; "a"; "b"; "c"; "_"]
    let syntactic = [s'; s]
    let lexical = [a; b; c]

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(var a * (var b + var c));
      ]

    let scanner =
      Production.[
        make (u, a) (codes "x");
        make (u, b) (codes "y");
        make (u, c) (codes "y");
      ]

    let tests = [
      Test.{
        name = "base";
        input = [
          code "x";
          code "y";
          eof
        ];
        trace = Trace.[
            order (vertex 0 0) a;
            expand (vertex 0 0) (vertex 7 0);
            read (code "x") (vertex 0 0) (vertex 7 0) (vertex 8 1);
            predict (vertex 8 1) (vertex 0 0) [a];
            shift (u, a) (vertex 8 1) (vertex 0 0) (vertex 0 0) (vertex 3 1);
            order (vertex 3 1) b;
            order (vertex 3 1) c;
            expand (vertex 3 1) (vertex 5 1);
            read (code "y") (vertex 3 1) (vertex 5 1) (vertex 6 2);
            predict (vertex 6 2) (vertex 3 1) [b; c];
            shift (u, b) (vertex 6 2) (vertex 3 1) (vertex 3 1) (vertex 4 2);
            shift (u, c) (vertex 6 2) (vertex 3 1) (vertex 3 1) (vertex 4 2);
            reduce (u, s) (vertex 4 2) (vertex 0 0) (vertex 1 2);
            load eof (vertex 1 2) (vertex 2 3);
            reduce (u, s) (vertex 4 2) (vertex 0 0) (vertex 1 2);
          ]
      };
    ]
  end)

(* Test of shift, which has to be performed from a non-root node *)
module Noncanonical = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; n0; n1; n2; a; l; u] = variables Vector.["S'"; "S"; "N0"; "N1"; "N2"; "a"; "l"; "_"]
    let syntactic = [s'; s; n0; n1; n2]
    let lexical = [a; l]

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(var n0 * var l);
        make (u, n0) (var n1);
        make (u, n1) (var n2);
        make (u, n2) (var a);
      ]

    let scanner =
      Production.[
        make (u, a) (codes "x");
        make (u, l) (codes "l");
      ]

    let tests = [
      Test.{
        name = "base";
        input = [
          code "x";
          code "l";
          eof
        ];
        trace = Trace.[
            order (vertex 0 0) a;
            expand (vertex 0 0) (vertex 10 0);
            read (code "x") (vertex 0 0) (vertex 10 0) (vertex 11 1);
            predict (vertex 11 1) (vertex 0 0) [a];
            shift (u, a) (vertex 11 1) (vertex 0 0) (vertex 0 0) (vertex 9 1);
            expand (vertex 9 1) (vertex 5 1);
            read (code "l") (vertex 9 1) (vertex 5 1) (vertex 6 2);
            predict (vertex 6 2) (vertex 9 1) [l];
            reduce (u, n2) (vertex 9 1) (vertex 0 0) (vertex 8 1);
            reduce (u, n1) (vertex 8 1) (vertex 0 0) (vertex 7 1);
            reduce (u, n0) (vertex 7 1) (vertex 0 0) (vertex 3 1);
            order (vertex 3 1) l;
            shift (u, l) (vertex 6 2) (vertex 9 1) (vertex 3 1) (vertex 4 2);
            reduce (u, s) (vertex 4 2) (vertex 0 0) (vertex 1 2);
            load eof (vertex 1 2) (vertex 2 3);
          ]
      }
    ]
  end)

(* Two root nodes joining to a single node, from which a shift is performed *)
module Noncanonical2 = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; n0; n1; n2; a; b; l; u; p] = variables Vector.["S'"; "S"; "N0"; "N1"; "N2"; "a"; "b"; "l"; "_"; "%"]
    let syntactic = [s'; s; n0; n1; n2]
    let lexical = [a; b; l]

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(var n0 * var l);
        make (u, n0) (var n1);
        make (u, n1) (var n2);
        make (u, n2) (var a);
        make (p, n2) (var b);
      ]

    let scanner =
      Production.[
        make (u, a) (codes "x");
        make (u, b) (codes "x");
        make (u, l) (codes "l");
      ]

    let tests = [
      Test.{
        name = "base";
        input = [
          code "x";
          code "l";
          eof
        ];
        trace = Trace.[
            order (vertex 0 0) a;
            order (vertex 0 0) b;
            expand (vertex 0 0) (vertex 11 0);
            read (code "x") (vertex 0 0) (vertex 11 0) (vertex 12 1);
            predict (vertex 12 1) (vertex 0 0) [a; b];
            shift (u, a) (vertex 12 1) (vertex 0 0) (vertex 0 0) (vertex 9 1);
            expand (vertex 9 1) (vertex 5 1);
            shift (u, b) (vertex 12 1) (vertex 0 0) (vertex 0 0) (vertex 10 1);
            expand (vertex 10 1) (vertex 5 1);
            read (code "l") (vertex 9 1) (vertex 5 1) (vertex 6 2);
            read (code "l") (vertex 10 1) (vertex 5 1) (vertex 6 2);
            predict (vertex 6 2) (vertex 9 1) [l];
            reduce (u, n2) (vertex 9 1) (vertex 0 0) (vertex 8 1);
            reduce (u, n1) (vertex 8 1) (vertex 0 0) (vertex 7 1);
            reduce (u, n0) (vertex 7 1) (vertex 0 0) (vertex 3 1);
            order (vertex 3 1) l;
            predict (vertex 6 2) (vertex 10 1) [l];
            reduce (p, n2) (vertex 10 1) (vertex 0 0) (vertex 8 1);
            shift (u, l) (vertex 6 2) (vertex 9 1) (vertex 3 1) (vertex 4 2);
            shift (u, l) (vertex 6 2) (vertex 10 1) (vertex 3 1) (vertex 4 2);
            reduce (u, s) (vertex 4 2) (vertex 0 0) (vertex 1 2);
            load eof (vertex 1 2) (vertex 2 3);
          ]
      }
    ]
  end)

(* Similar as before, except that one of the root nodes is missing, in turn, due to circumstances *)
module Noncanonical3 = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; n0; n1; n2; a; b; l; u; p] = variables Vector.["S'"; "S"; "N0"; "N1"; "N2"; "a"; "b"; "l"; "_"; "%"]
    let syntactic = [s'; s; n0; n1; n2]
    let lexical = [a; b; l]

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(var n0 * plus (var l));
        make (u, n0) (var n1);
        make (u, n1) (var n2);
        make (u, n2) (var a);
        make (p, n2) (var b);
      ]

    let scanner =
      Production.[
        make (u, a) (codes "x");
        make (u, b) R.(codes "x" * plus (codes "l"));
        make (u, l) (codes "l");
      ]

    let tests = [
      Test.{
        name = "base";
        input = [
          code "x";
          code "l";
          code "l";
          eof
        ];
        trace = Trace.[
            order (vertex 0 0) a;
            order (vertex 0 0) b;
            expand (vertex 0 0) (vertex 11 0);
            read (code "x") (vertex 0 0) (vertex 11 0) (vertex 12 1);
            predict (vertex 12 1) (vertex 0 0) [a; b];
            shift (u, a) (vertex 12 1) (vertex 0 0) (vertex 0 0) (vertex 9 1);
            expand (vertex 9 1) (vertex 5 1);
            read (code "l") (vertex 0 0) (vertex 12 1) (vertex 13 2);
            read (code "l") (vertex 9 1) (vertex 5 1) (vertex 6 2);
            shift (u, b) (vertex 13 2) (vertex 0 0) (vertex 0 0) (vertex 10 2);
            expand (vertex 10 2) (vertex 5 2);
            predict (vertex 6 2) (vertex 9 1) [l];
            reduce (u, n2) (vertex 9 1) (vertex 0 0) (vertex 8 1);
            reduce (u, n1) (vertex 8 1) (vertex 0 0) (vertex 7 1);
            reduce (u, n0) (vertex 7 1) (vertex 0 0) (vertex 3 1);
            order (vertex 3 1) l;
            shift (u, l) (vertex 6 2) (vertex 9 1) (vertex 3 1) (vertex 4 2);
            order (vertex 4 2) l;
            expand (vertex 4 2) (vertex 5 2);
            read (code "l") (vertex 0 0) (vertex 13 2) (vertex 13 3);
            read (code "l") (vertex 4 2) (vertex 5 2) (vertex 6 3);
            read (code "l") (vertex 10 2) (vertex 5 2) (vertex 6 3);
            predict (vertex 6 3) (vertex 4 2) [l];
            predict (vertex 6 3) (vertex 10 2) [l];
            reduce (p, n2) (vertex 10 2) (vertex 0 0) (vertex 8 2);
            reduce (u, n1) (vertex 8 2) (vertex 0 0) (vertex 7 2);
            reduce (u, n0) (vertex 7 2) (vertex 0 0) (vertex 3 2);
            order (vertex 3 2) l;
            shift (u, l) (vertex 6 3) (vertex 4 2) (vertex 4 2) (vertex 4 3);
            order (vertex 4 3) l;
            expand (vertex 4 3) (vertex 5 3);
            shift (u, l) (vertex 6 3) (vertex 10 2) (vertex 3 2) (vertex 4 3);
            reduce (u, s) (vertex 4 3) (vertex 0 0) (vertex 1 3);
            load eof (vertex 1 3) (vertex 2 4);
            reduce (u, s) (vertex 4 3) (vertex 0 0) (vertex 1 3);
          ]
      }
    ]
  end)

(* Repetition at the level of the parser and the scanner, and nullable productions *)
(* Just something complex to see if something has falen though the cracks when testing basic operations *)
(* This example also has complex order connections *)
(* DISCOVERED: repeated null operations *)
(* DISCOVERED: order loops, null actions connect two root vertices  *)
module Noncanonical4 = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; n0; n1; n2; a; b; l; u; p] = variables Vector.["S'"; "S"; "N0"; "N1"; "N2"; "a"; "b"; "l"; "_"; "%"]
    let syntactic = [s'; s; n0; n1; n2]
    let lexical = [a; b; l]

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(var l * var n0 * var l * var s + null);
        make (u, n0) (var n1);
        make (u, n1) (var n2);
        make (u, n2) (var a);
        make (p, n2) (var b);
      ]

    let scanner =
      Production.[
        make (u, a) (codes "x");
        make (u, b) R.(codes "x" * codes "x");
        make (u, l) R.(star (codes "x"));
      ]

    let tests = [
      Test.{
        name = "base";
        input = [
          code "x";
          code "x";
          eof
        ];
        trace = Trace.[
            order (vertex 0 0) l;
            expand (vertex 0 0) (vertex 16 0);
            read (code "x") (vertex 0 0) (vertex 16 0) (vertex 17 1);
            predict (vertex 17 1) (vertex 0 0) [a; b; l];
            reduce (u, l) (vertex 0 0) (vertex 0 0) (vertex 3 0);
            order (vertex 3 0) a;
            order (vertex 3 0) b;
            shift (u, a) (vertex 17 1) (vertex 0 0) (vertex 3 0) (vertex 11 1);
            expand (vertex 11 1) (vertex 16 1);
            shift (u, l) (vertex 17 1) (vertex 0 0) (vertex 0 0) (vertex 3 1);
            order (vertex 3 1) a;
            order (vertex 3 1) b;
            expand (vertex 3 1) (vertex 13 1);
            read (code "x") (vertex 0 0) (vertex 17 1) (vertex 18 2);
            read (code "x") (vertex 11 1) (vertex 16 1) (vertex 17 2);
            read (code "x") (vertex 3 1) (vertex 13 1) (vertex 14 2);
            shift (u, b) (vertex 18 2) (vertex 0 0) (vertex 3 0) (vertex 12 2);
            expand (vertex 12 2) (vertex 16 2);
            shift (u, l) (vertex 18 2) (vertex 0 0) (vertex 0 0) (vertex 3 2);
            order (vertex 3 2) a;
            order (vertex 3 2) b;
            expand (vertex 3 2) (vertex 13 2);
            predict (vertex 17 2) (vertex 11 1) [a; l];
            reduce (u, n2) (vertex 11 1) (vertex 3 0) (vertex 10 1);
            reduce (u, n1) (vertex 10 1) (vertex 3 0) (vertex 9 1);
            reduce (u, n0) (vertex 9 1) (vertex 3 0) (vertex 4 1);
            order (vertex 4 1) l;
            reduce (u, l) (vertex 4 1) (vertex 4 1) (vertex 5 1);
            order (vertex 5 1) l;
            reduce (u, l) (vertex 5 1) (vertex 5 1) (vertex 3 1);
            shift (u, a) (vertex 17 2) (vertex 11 1) (vertex 3 1) (vertex 11 2);
            expand (vertex 11 2) (vertex 16 2);
            shift (u, l) (vertex 17 2) (vertex 11 1) (vertex 4 1) (vertex 5 2);
            order (vertex 5 2) l;
            expand (vertex 5 2) (vertex 16 2);
            shift (u, l) (vertex 17 2) (vertex 11 1) (vertex 5 1) (vertex 3 2);
            predict (vertex 14 2) (vertex 3 1) [a];
            shift (u, a) (vertex 14 2) (vertex 3 1) (vertex 3 1) (vertex 11 2);
            reduce (p, n2) (vertex 12 2) (vertex 3 0) (vertex 10 2);
            reduce (u, n1) (vertex 10 2) (vertex 3 0) (vertex 9 2);
            reduce (u, n0) (vertex 9 2) (vertex 3 0) (vertex 4 2);
            order (vertex 4 2) l;
            reduce (u, s) (vertex 4 2) (vertex 0 0) (vertex 1 2);
            load eof (vertex 1 2) (vertex 2 3);
            reduce (u, l) (vertex 4 2) (vertex 4 2) (vertex 5 2);
            reduce (u, n2) (vertex 11 2) (vertex 3 1) (vertex 10 2);
            reduce (u, n1) (vertex 10 2) (vertex 3 1) (vertex 9 2);
            reduce (u, n0) (vertex 9 2) (vertex 3 1) (vertex 4 2);
            reduce (u, s) (vertex 4 2) (vertex 0 0) (vertex 1 2);
            reduce (u, s) (vertex 4 2) (vertex 5 1) (vertex 6 2);
            reduce (u, s) (vertex 6 2) (vertex 0 0) (vertex 1 2);
            reduce (u, l) (vertex 4 2) (vertex 4 2) (vertex 5 2);
            reduce (u, s) (vertex 5 2) (vertex 0 0) (vertex 1 2);
            reduce (u, s) (vertex 5 2) (vertex 5 2) (vertex 6 2);
            reduce (u, l) (vertex 5 2) (vertex 5 2) (vertex 3 2);
          ]
      }
    ]
  end)

(* Two shifts performed from a single root at different "heights" *)
module Noncanonical5 = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context
    let Vector.[s'; s; n0; n1; n2; a; l0; l1; u] = variables Vector.["S'"; "S"; "N0"; "N1"; "N2"; "a"; "l0"; "l1"; "_"]
    let syntactic = [s'; s; n0; n1; n2]
    let lexical = [a; l0; l1]

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(var n0 * opt (var l0));
        make (u, n0) R.(var n1 * opt (var l1));
        make (u, n1) (var n2);
        make (u, n2) (var a);
      ]

    let scanner =
      Production.[
        make (u, a) (codes "x");
        make (u, l0) R.(codes "l" * codes "l");
        make (u, l1) (codes "l");
      ]

    let tests = [
      Test.{
        name = "base";
        input = [
          code "x";
          code "l";
          code "l";
          eof
        ];
        trace = Trace.[
            order (vertex 0 0) a;
            expand (vertex 0 0) (vertex 14 0);
            read (code "x") (vertex 0 0) (vertex 14 0) (vertex 15 1);
            predict (vertex 15 1) (vertex 0 0) [a];
            shift (u, a) (vertex 15 1) (vertex 0 0) (vertex 0 0) (vertex 13 1);
            expand (vertex 13 1) (vertex 16 1);
            read (code "l") (vertex 13 1) (vertex 16 1) (vertex 17 2);
            predict (vertex 17 2) (vertex 13 1) [l0; l1];
            reduce (u, n2) (vertex 13 1) (vertex 0 0) (vertex 12 1);
            reduce (u, n1) (vertex 12 1) (vertex 0 0) (vertex 8 1);
            order (vertex 8 1) l1;
            reduce (u, n0) (vertex 8 1) (vertex 0 0) (vertex 3 1);
            order (vertex 3 1) l0;
            shift (u, l1) (vertex 17 2) (vertex 13 1) (vertex 8 1) (vertex 9 2);
            expand (vertex 9 2) (vertex 5 2);
            read (code "l") (vertex 13 1) (vertex 17 2) (vertex 7 3);
            read (code "l") (vertex 9 2) (vertex 5 2) (vertex 6 3);
            shift (u, l0) (vertex 7 3) (vertex 13 1) (vertex 3 1) (vertex 4 3);
            reduce (u, s) (vertex 4 3) (vertex 0 0) (vertex 1 3);
            load eof (vertex 1 3) (vertex 2 4);
          ];
      }
    ]
  end)

(* "arbitrary" character lookahead, nothing particularly special for a parser with a scanner *)
module Lookahead = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; b; c; d; l0; l1; u] = variables Vector.["S'"; "S"; "A"; "B"; "C"; "d"; "l0"; "l1"; "_"]
    let syntactic = [s'; s; a; b; c]
    let lexical = [d; l0; l1]

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) (var a);
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

    let tests = [
      Test.{
        name = "base";
        input = [
          code "x";
          code "x";
          code "x";
          code "x";
          code "a";
          eof
        ];
        trace = Trace.[
            order (vertex 0 0) d;
            expand (vertex 0 0) (vertex 14 0);
            read (code "x") (vertex 0 0) (vertex 14 0) (vertex 15 1);
            predict (vertex 15 1) (vertex 0 0) [d];
            shift (u, d) (vertex 15 1) (vertex 0 0) (vertex 0 0) (vertex 13 1);
            expand (vertex 13 1) (vertex 16 1);
            read (code "x") (vertex 13 1) (vertex 16 1) (vertex 17 2);
            read (code "x") (vertex 13 1) (vertex 17 2) (vertex 17 3);
            read (code "x") (vertex 13 1) (vertex 17 3) (vertex 17 4);
            read (code "a") (vertex 13 1) (vertex 17 4) (vertex 8 5);
            predict (vertex 8 5) (vertex 13 1) [l0];
            reduce (u, b) (vertex 13 1) (vertex 0 0) (vertex 4 1);
            order (vertex 4 1) l0;
            shift (u, l0) (vertex 8 5) (vertex 13 1) (vertex 4 1) (vertex 5 5);
            reduce (u, a) (vertex 5 5) (vertex 0 0) (vertex 3 5);
            reduce (u, s) (vertex 3 5) (vertex 0 0) (vertex 1 5);
            load eof (vertex 1 5) (vertex 2 6);
          ]
      }
    ]
  end)

(* fractional lookahead, search for d l0 l1 is performed from the same root *)
(* DISCOVERED the lookahead for noncanonical states needs to be recomputed *)
module Lookahead2 = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
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

    let tests = [
      Test.{
        name = "base";
        input = [
          code "x";
          code "x";
          code "x";
          code "x";
          code "a";
          eof
        ];
        trace = Trace.[
            order (vertex 0 0) d;
            expand (vertex 0 0) (vertex 15 0);
            read (code "x") (vertex 0 0) (vertex 15 0) (vertex 16 1);
            predict (vertex 16 1) (vertex 0 0) [d];
            shift (u, d) (vertex 16 1) (vertex 0 0) (vertex 0 0) (vertex 13 1);
            order (vertex 13 1) d;
            expand (vertex 13 1) (vertex 17 1);
            read (code "x") (vertex 13 1) (vertex 17 1) (vertex 18 2);
            predict (vertex 18 2) (vertex 13 1) [d; l0; l1];
            reduce (u, b) (vertex 13 1) (vertex 0 0) (vertex 4 1);
            order (vertex 4 1) l0;
            reduce (u, c) (vertex 13 1) (vertex 0 0) (vertex 9 1);
            order (vertex 9 1) l1;
            shift (u, d) (vertex 18 2) (vertex 13 1) (vertex 13 1) (vertex 14 2);
            read (code "x") (vertex 13 1) (vertex 18 2) (vertex 19 3);
            read (code "x") (vertex 13 1) (vertex 19 3) (vertex 19 4);
            read (code "a") (vertex 13 1) (vertex 19 4) (vertex 8 5);
            shift (u, l0) (vertex 8 5) (vertex 13 1) (vertex 4 1) (vertex 5 5);
            reduce (u, a) (vertex 5 5) (vertex 0 0) (vertex 3 5);
            reduce (u, s) (vertex 3 5) (vertex 0 0) (vertex 1 5);
            load eof (vertex 1 5) (vertex 2 6);
          ]
      }
    ]
  end)

(*module Example = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; b; c; d; u] = variables Vector.["S'"; "S"; "A"; "B"; "C"; "D"; "_"]
    let syntactic = [s'; s; a; b]
    let lexical = [c; d]

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(var a * var c + var b * var d);
        make (u, a) (codes "x");
        make (u, b) (codes "x");
      ]

    let scanner =
      Production.[
        make (u, c) R.(codes "x" * codes "x" * codes "a");
        make (u, d) R.(codes "x" * codes "x" * codes "b");
      ]

    let tests = [
      Test.{
        name = "base";
        input = [
          code "x";
          code "x";
          code "x";
          code "a";
          eof
        ];
        trace = Trace.[
            expand (vertex 0 0) (vertex 15 0);
          ]
      }
    ]
  end)*)

(*module Lookahead3 = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; b; c; d; e; l0; l1; u] = variables Vector.["S'"; "S"; "A"; "B"; "C"; "d"; "e"; "l0"; "l1"; "_"]
    let syntactic = [s'; s; a; b; c]
    let lexical = [d; e; l0; l1]

    let start = s'

    let parser =
      Production.[
        make (u, s') R.(var s * plus eof);
        make (u, s) R.(var a + var d * var d);
        make (u, a) R.(var b * var l0 + var c * var l1);
        make (u, b) (var e);
        make (u, c) (var e);
      ]

    let scanner =
      Production.[
        make (u, d) (codes "x");
        make (u, e) (codes "x");
        make (u, l0) R.(plus (codes "x") * codes "a");
        make (u, l1) R.(plus (codes "x") * codes "b");
      ]

    let tests = [
      Test.{
        name = "base";
        input = [
          code "x";
          code "x";
          code "x";
          code "x";
          code "a";
          eof
        ];
        trace = Trace.[
            expand (vertex 0 0) (vertex 8 0);
            reduce (u, s) (vertex 0 0) (vertex 0 0) (vertex 1 0);
            load eof (vertex 1 0) (vertex 2 1);
          ]
      }
    ]
  end)
   *)

let () =
  Alcotest.run "parser" [
    "noop", test_cases Noop.driver Noop.tests;
    "null", test_cases Null.driver Null.tests;
    "null_reduce", test_cases Null_reduce.driver Null_reduce.tests;
    "null_shift", test_cases Null_shift.driver Null_shift.tests;
    "load", test_cases Load.driver Load.tests;
    "load2", test_cases Load2.driver Load2.tests;
    "load_reduce", test_cases Load_reduce.driver Load_reduce.tests;
    "multilabel_load_reduce", test_cases Multilabel_load_reduce.driver Multilabel_load_reduce.tests;
    "shift", test_cases Shift.driver Shift.tests;
    "shift_reduce", test_cases Shift_reduce.driver Shift_reduce.tests;
    "shift_reduce2", test_cases Shift_reduce2.driver Shift_reduce2.tests;
    "multilabel_shift_reduce", test_cases Multilabel_shift_reduce.driver Multilabel_shift_reduce.tests;
    "same_label_reduce", test_cases Same_label_reduce.driver Same_label_reduce.tests;
    "right_nulled", test_cases Right_nulled.driver Right_nulled.tests;
    (*"right_nulled2", test_cases Right_nulled2.driver Right_nulled2.tests;*)
    "repeat_load", test_cases Repeat_load.driver Repeat_load.tests;
    "left_recursion_load", test_cases Left_recursion_load.driver Left_recursion_load.tests;
    "right_recursion_load", test_cases Right_recursion_load.driver Right_recursion_load.tests;
    "nest_load", test_cases Nest_load.driver Nest_load.tests;
    "middle", test_cases Middle.driver Middle.tests;
    "left_recursion_shift", test_cases Left_recursion_shift.driver Left_recursion_shift.tests;
    "right_recursion_shift", test_cases Right_recursion_shift.driver Right_recursion_shift.tests;
    "nest_shift", test_cases Nest_shift.driver Nest_shift.tests;
    "predict", test_cases Predict.driver Predict.tests;
    "noncanonical", test_cases Noncanonical.driver Noncanonical.tests;
    "noncanonical2", test_cases Noncanonical2.driver Noncanonical2.tests;
    "noncanonical3", test_cases Noncanonical3.driver Noncanonical3.tests;
    "noncanonical4", test_cases Noncanonical4.driver Noncanonical4.tests;
    "noncanonical5", test_cases Noncanonical5.driver Noncanonical5.tests;
    "lookahead", test_cases Lookahead.driver Lookahead.tests;
    "lookahead2", test_cases Lookahead2.driver Lookahead2.tests;
    (*"example", test_cases Example.driver Example.tests;*)
  ]

