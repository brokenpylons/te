open Te_bot
open Te_core
module T = Types

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
              load eof (vertex [0] 0) (vertex [1] 1)
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
              load eof (vertex [0] 0) (vertex [1] 1)
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
            load (code "a") (vertex [0] 0) (vertex [1] 1);
            load eof (vertex [1] 1) (vertex [2] 2);
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
            load (code "a") (vertex [0] 0) (vertex [1] 1);
            load (code "b") (vertex [1] 1) (vertex [2] 2);
            load eof (vertex [2] 2) (vertex [3] 3);
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
            reduce (u, s) (vertex [0] 0) (vertex [0] 0) (vertex [1] 0);
            load eof (vertex [1] 0) (vertex [2] 1);
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
            order (vertex [0] 0) s;
            load (code "a") (vertex [0] 0) (vertex [1] 1);
            reduce (u, s) (vertex [1] 1) (vertex [0] 0) (vertex [2] 1);
            load eof (vertex [2] 1) (vertex [3] 2);
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
            expand (vertex [0] 0) (vertex [3] 0) ;
            read (code "a") (vertex [0] 0) (vertex [3] 0) (vertex [4] 1);
            predict (vertex [0] 0) [a];
            order (vertex [0] 0) a;
            shift (u, a) (vertex [0] 0) (vertex [0] 0) (vertex [1] 1);
            load eof (vertex [1] 1) (vertex [2] 2);
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
            expand (vertex [0] 0) (vertex [4] 0) ;
            read (code "a") (vertex [0] 0) (vertex [4] 0) (vertex [5] 1);
            predict (vertex [0] 0) [a];
            order (vertex [0] 0) s;
            order (vertex [0] 0) a;
            shift (u, a) (vertex [0] 0) (vertex [0] 0) (vertex [3] 1);
            reduce (u, s) (vertex [3] 1) (vertex [0] 0) (vertex [1] 1);
            load eof (vertex [1] 1) (vertex [2] 2);
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
            expand (vertex [0] 0) (vertex [5] 0) ;
            read (code "a") (vertex [0] 0) (vertex [5] 0) (vertex [6] 1);
            predict (vertex [0] 0) [a];
            order (vertex [0] 0) s;
            order (vertex [0] 0) p;
            order (vertex [0] 0) a;
            shift (u, a) (vertex [0] 0) (vertex [0] 0) (vertex [4] 1);
            reduce (u, p) (vertex [4] 1) (vertex [0] 0) (vertex [3] 1);
            reduce (u, s) (vertex [3] 1) (vertex [0] 0) (vertex [1] 1);
            load eof (vertex [1] 1) (vertex [2] 2);
          ]
      }
    ]
  end)

module Right_nulled = Spec.Test(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let Vector.[s'; s; a; u] = variables Vector.["S'"; "S"; "a"; "_"]
    let syntactic = [s'; s]
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
            order (vertex [0] 0) s;
            load (code "a") (vertex [0] 0) (vertex [1] 1);
            reduce (u, s) (vertex [1] 1) (vertex [0] 0) (vertex [3] 1);
            load eof (vertex [3] 1) (vertex [4] 2);
            reduce (u, a) (vertex [1] 1) (vertex [1] 1) (vertex [2] 1);
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
            reduce (u, s) (vertex [0] 0) (vertex [0] 0) (vertex [1] 0);
            load eof (vertex [1] 0) (vertex [3] 1);
          ]
      };
      Test.{
        name = "n1";
        input = [
          code "a";
          eof
        ];
        trace = Trace.[
            order (vertex [0] 0) s;
            reduce (u, s) (vertex [0] 0) (vertex [0] 0) (vertex [1] 0);
            load (code "a") (vertex [1] 0) (vertex [2] 1);
            reduce (u, s) (vertex [2] 1) (vertex [0] 0) (vertex [1] 1);
            load eof (vertex [1] 1) (vertex [3] 2);
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
            order (vertex [0] 0) s;
            reduce (u, s) (vertex [0] 0) (vertex [0] 0) (vertex [1] 0);
            load (code "a") (vertex [1] 0) (vertex [2] 1);
            reduce (u, s) (vertex [2] 1) (vertex [0] 0) (vertex [1] 1);
            load (code "a") (vertex [1] 1) (vertex [2] 2);
            reduce (u, s) (vertex [2] 2) (vertex [0] 0) (vertex [1] 2);
            load eof (vertex [1] 2) (vertex [3] 3);
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
            order (vertex [0] 0) s;
            reduce (u, s) (vertex [0] 0) (vertex [0] 0) (vertex [1] 0);
            load (code "a") (vertex [1] 0) (vertex [2] 1);
            reduce (u, s) (vertex [2] 1) (vertex [0] 0) (vertex [1] 1);
            load (code "a") (vertex [1] 1) (vertex [2] 2);
            reduce (u, s) (vertex [2] 2) (vertex [0] 0) (vertex [1] 2);
            load (code "a") (vertex [1] 2) (vertex [2] 3);
            reduce (u, s) (vertex [2] 3) (vertex [0] 0) (vertex [1] 3);
            load eof (vertex [1] 3) (vertex [3] 4);
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
            reduce (u, s) (vertex [0] 0) (vertex [0] 0) (vertex [3] 0);
            load eof (vertex [3] 0) (vertex [4] 1);
          ]
      };
      Test.{
        name = "n1";
        input = [
          code "a";
          eof
        ];
        trace = Trace.[
            order (vertex [0] 0) s;
            load (code "a") (vertex [0] 0) (vertex [1] 1);
            reduce (u, s) (vertex [1] 1) (vertex [0] 0) (vertex [3] 1);
            load eof (vertex [3] 1) (vertex [4] 2);
            reduce (u, s) (vertex [1] 1) (vertex [1] 1) (vertex [2] 1);
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
            order (vertex [0] 0) s;
            load (code "a") (vertex [0] 0) (vertex [1] 1);
            order (vertex [1] 1) s;
            load (code "a") (vertex [1] 1) (vertex [1] 2);
            reduce (u, s) (vertex [1] 2) (vertex [1] 1) (vertex [2] 2);
            reduce (u, s) (vertex [2] 2) (vertex [0] 0) (vertex [3] 2);
            load eof (vertex [3] 2) (vertex [4] 3);
            reduce (u, s) (vertex [1] 2) (vertex [1] 2) (vertex [2] 2);
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
            order (vertex [0] 0) s;
            load (code "a") (vertex [0] 0) (vertex [1] 1);
            order (vertex [1] 1) s;
            load (code "a") (vertex [1] 1) (vertex [1] 2);
            order (vertex [1] 2) s;
            load (code "a") (vertex [1] 2) (vertex [1] 3);
            reduce (u, s) (vertex [1] 3) (vertex [1] 2) (vertex [2] 3);
            reduce (u, s) (vertex [2] 3) (vertex [1] 1) (vertex [2] 3);
            reduce (u, s) (vertex [2] 3) (vertex [0] 0) (vertex [3] 3);
            load eof (vertex [3] 3) (vertex [4] 4);
            reduce (u, s) (vertex [1] 3) (vertex [1] 3) (vertex [2] 3);
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
            reduce (u, s) (vertex [0] 0) (vertex [0] 0) (vertex [4] 0);
            load eof (vertex [4] 0) (vertex [5] 1);
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
            order (vertex [0] 0) s;
            load (code "a") (vertex [0] 0) (vertex [1] 1);
            reduce (u, s) (vertex [1] 1) (vertex [1] 1) (vertex [2] 1);
            load (code "b") (vertex [2] 1) (vertex [3] 2);
            reduce (u, s) (vertex [3] 2) (vertex [0] 0) (vertex [4] 2);
            load eof (vertex [4] 2) (vertex [5] 3);
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
            order (vertex [0] 0) s;
            load (code "a") (vertex [0] 0) (vertex [1] 1);
            order (vertex [1] 1) s;
            load (code "a") (vertex [1] 1) (vertex [1] 2);
            reduce (u, s) (vertex [1] 2) (vertex [1] 2) (vertex [2] 2);
            load (code "b") (vertex [2] 2) (vertex [3] 3);
            reduce (u, s) (vertex [3] 3) (vertex [1] 1) (vertex [2] 3);
            load (code "b") (vertex [2] 3) (vertex [3] 4);
            reduce (u, s) (vertex [3] 4) (vertex [0] 0) (vertex [4] 4);
            load eof (vertex [4] 4) (vertex [5] 5);
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
            reduce (u, s) (vertex [0] 0) (vertex [0] 0) (vertex [4] 0);
            load eof (vertex [4] 0) (vertex [5] 1);
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
            order (vertex [0] 0) s;
            load (code "a") (vertex [0] 0) (vertex [1] 1);
            order (vertex [1] 1) s;
            reduce (u, s) (vertex [1] 1) (vertex [1] 1) (vertex [2] 1);
            load (code "a") (vertex [2] 1) (vertex [3] 2);
            load (code "a") (vertex [1] 1) (vertex [1] 2);
            reduce (u, s) (vertex [3] 2) (vertex [0] 0) (vertex [4] 2);
            load eof (vertex [4] 2) (vertex [5] 3);
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
            order (vertex [0] 0) s;
            load (code "a") (vertex [0] 0) (vertex [1] 1);
            order (vertex [1] 1) s;
            reduce (u, s) (vertex [1] 1) (vertex [1] 1) (vertex [2] 1);
            load (code "a") (vertex [2] 1) (vertex [3] 2);
            load (code "a") (vertex [1] 1) (vertex [1] 2);
            reduce (u, s) (vertex [3] 2) (vertex [0] 0) (vertex [4] 2);
            order (vertex [1] 2) s;
            reduce (u, s) (vertex [1] 2) (vertex [1] 2) (vertex [2] 2);
            load (code "a") (vertex [2] 2) (vertex [3] 3);
            load (code "a") (vertex [1] 2) (vertex [1] 3);
            reduce (u, s) (vertex [3] 3) (vertex [1] 1) (vertex [2] 3);
            load (code "a") (vertex [2] 3) (vertex [3] 4);
            order (vertex [1] 3) s;
            reduce (u, s) (vertex [1] 3) (vertex [1] 3) (vertex [2] 3);
            load (code "a") (vertex [2] 3) (vertex [3] 4);
            load (code "a") (vertex [1] 3) (vertex [1] 4);
            reduce (u, s) (vertex [3] 4) (vertex [0] 0) (vertex [4] 4);
            load eof (vertex [4] 4) (vertex [5] 5);
            reduce (u, s) (vertex [3] 4) (vertex [1] 2) (vertex [2] 4);
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
            expand (vertex [0] 0) (vertex [3] 0);
            reduce (u, s) (vertex [0] 0) (vertex [0] 0) (vertex [1] 0);
            load eof (vertex [1] 0) (vertex [5] 1);
          ]
      };
      Test.{
        name = "n1";
        input = [
          code "a";
          eof
        ];
        trace = Trace.[
            expand (vertex [0] 0) (vertex [3] 0);
            order (vertex [0] 0) s;
            reduce (u, s) (vertex [0] 0) (vertex [0] 0) (vertex [1] 0);
            order (vertex [1] 0) a;
            read (code "a") (vertex [0] 0) (vertex [3] 0) (vertex [4] 1);
            shift (u, a) (vertex [0] 0) (vertex [1] 0) (vertex [2] 1);
            expand (vertex [2] 1) (vertex [3] 1);
            reduce (u, s) (vertex [2] 1) (vertex [0] 0) (vertex [1] 1);
            load eof (vertex [1] 1) (vertex [5] 2);
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
            expand (vertex [0] 0) (vertex [3] 0);
            order (vertex [0] 0) s;
            reduce (u, s) (vertex [0] 0) (vertex [0] 0) (vertex [1] 0);
            order (vertex [1] 0) a;
            read (code "a") (vertex [0] 0) (vertex [3] 0) (vertex [4] 1);
            shift (u, a) (vertex [0] 0) (vertex [1] 0) (vertex [2] 1);
            expand (vertex [2] 1) (vertex [3] 1);
            reduce (u, s) (vertex [2] 1) (vertex [0] 0) (vertex [1] 1);
            order (vertex [1] 1) a;
            read (code "a") (vertex [2] 1) (vertex [3] 1) (vertex [4] 2);
            shift (u, a) (vertex [2] 1) (vertex [1] 1) (vertex [2] 2);
            expand (vertex [2] 2) (vertex [3] 2);
            reduce (u, s) (vertex [2] 2) (vertex [0] 0) (vertex [1] 2);
            load eof (vertex [1] 2) (vertex [5] 3);
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
            expand (vertex [0] 0) (vertex [3] 0);
            order (vertex [0] 0) s;
            reduce (u, s) (vertex [0] 0) (vertex [0] 0) (vertex [1] 0);
            order (vertex [1] 0) a;
            read (code "a") (vertex [0] 0) (vertex [3] 0) (vertex [4] 1);
            shift (u, a) (vertex [0] 0) (vertex [1] 0) (vertex [2] 1);
            expand (vertex [2] 1) (vertex [3] 1);
            reduce (u, s) (vertex [2] 1) (vertex [0] 0) (vertex [1] 1);
            order (vertex [1] 1) a;
            read (code "a") (vertex [2] 1) (vertex [3] 1) (vertex [4] 2);
            shift (u, a) (vertex [2] 1) (vertex [1] 1) (vertex [2] 2);
            expand (vertex [2] 2) (vertex [3] 2);
            reduce (u, s) (vertex [2] 2) (vertex [0] 0) (vertex [1] 2);
            order (vertex [1] 2) a;
            read (code "a") (vertex [2] 2) (vertex [3] 2) (vertex [4] 3);
            shift (u, a) (vertex [2] 2) (vertex [1] 2) (vertex [2] 3);
            expand (vertex [2] 3) (vertex [3] 3);
            reduce (u, s) (vertex [2] 3) (vertex [0] 0) (vertex [1] 3);
            load eof (vertex [1] 3) (vertex [5] 4);
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
            expand (vertex [0] 0) (vertex [5] 0);
            reduce (u, s) (vertex [0] 0) (vertex [0] 0) (vertex [1] 0);
            load eof (vertex [1] 0) (vertex [2] 1);
          ]
      };
      Test.{
        name = "n1";
        input = [
          code "a";
          eof
        ];
        trace = Trace.[
            expand (vertex [0] 0) (vertex [5] 0);
            reduce (u, s) (vertex [0] 0) (vertex [0] 0) (vertex [1] 0);
            order (vertex [0] 0) s;
            order (vertex [0] 0) a;
            read (code "a") (vertex [0] 0) (vertex [5] 0) (vertex [6] 1);
            shift (u, a) (vertex [0] 0) (vertex [0] 0) (vertex [3] 1);





            load eof (vertex [1] 0) (vertex [2] 1);
          ]
      };
    ]
  end)

let () =
  Alcotest.run "parser" [
    "noop", test_cases Noop.driver Noop.tests;
    "null", test_cases Null.driver Null.tests;
    "null_redice", test_cases Null_reduce.driver Null_reduce.tests;
    "load", test_cases Load.driver Load.tests;
    "load2", test_cases Load2.driver Load2.tests;
    "load_reduce", test_cases Load_reduce.driver Load_reduce.tests;
    "shift", test_cases Shift.driver Shift.tests;
    "shift_reduce", test_cases Shift_reduce.driver Shift_reduce.tests;
    "shift_reduce2", test_cases Shift_reduce2.driver Shift_reduce2.tests;
    "right_nulled", test_cases Right_nulled.driver Right_nulled.tests;
    "left_recursion_load", test_cases Left_recursion_load.driver Left_recursion_load.tests;
    "right_recursion_load", test_cases Right_recursion_load.driver Right_recursion_load.tests;
    "nest_load", test_cases Nest_load.driver Nest_load.tests;
    "middle", test_cases Middle.driver Middle.tests;
    "left_recursion_shift", test_cases Left_recursion_shift.driver Left_recursion_shift.tests;
    "right_recursion_shift", test_cases Right_recursion_shift.driver Right_recursion_shift.tests;
  ]

