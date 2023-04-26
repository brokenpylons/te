open Te_core
module T = Types

module R = Re.Porcelan(Re.Abstract)

let c x = Tn.Lits.(Codes  (T.Codes.of_string x))

let test_lits' () =
  Alcotest.check Alcotest.bool "is subset" false (Tn.R.is_nullable (Tn.R.derivative' [c "a"] R.(diff any (lits (c "ab")))))

let () =
  Alcotest.run "fa" [
    "lits'", [
      Alcotest.test_case "normal" `Quick test_lits';
    ]
  ]
