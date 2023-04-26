open Te_core
module T = Types

let c = T.Code.of_int

let test_of_string () =
  Alcotest.check (module T.Codes) "equal" (T.Codes.of_list [c 0x41; c 0x7b; c 0x17E]) (T.Codes.of_string "A{ž")

let () =
  Alcotest.run "code" [
    "code", [
      Alcotest.test_case "of_string" `Quick test_of_string;
    ]
  ]
