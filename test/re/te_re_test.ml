open Te_core
module T = Types

module R = struct
  include Re.Abstract
  include Re.Concrete(T.Codes)
end

let codes x = R.lits (T.Codes.of_string x)

let union () =
  Alcotest.check (module R) "assoc" 
    R.(codes "a" + codes "b")
    (R.simplify R.(codes "a" + codes "b"));
  Alcotest.check (module R) "assoc"
    R.(codes "a" + codes "b" + codes "c")
    (R.simplify R.(codes "a" + (codes "b" + codes "c")));
  Alcotest.check (module R) "assoc"
    R.(codes "a" + codes "b" + codes "c" + codes "d")
    (R.simplify R.(codes "a" + (codes "b" + (codes "c" + codes "d"))));
  Alcotest.check (module R) "assoc"
    R.(codes "a" + codes "b" + codes "c" + codes "d")
    (R.simplify R.((codes "a" + codes "b") + (codes "c" + codes "d")));
  Alcotest.check (module R) "comm"
    R.(codes "a" + codes "b")
    (R.simplify R.(codes "b" + codes "a"));
  Alcotest.check (module R) "equal"
    R.(codes "a" + codes "b")
    (R.simplify R.(codes "a" + codes "b" + codes "a"));
  Alcotest.check (module R) "equal"
    R.(codes "a" * codes "c")
    (R.simplify R.((codes "a" + codes "a") * codes "c"))

let () =
  Alcotest.run "re" [
    "rules", [
      Alcotest.test_case "union" `Quick union;
    ]
  ]

