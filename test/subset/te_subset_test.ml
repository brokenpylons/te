module Set = struct
  include Te_bot.Balanced_binary_tree.Set.Size(Int)
  (*let pp = pp Fmt.int*)
end

let test_normal () =
  Alcotest.check Alcotest.bool "is subset" true Set.(subset (1 & 2 & empty) (1 & 2 & 3 & empty))

let test_left_empty () =
  Alcotest.check Alcotest.bool "is subset" true Set.(subset (empty) (1 & empty))

let test_right_empty () =
  Alcotest.check Alcotest.bool "is subset" false Set.(subset (1 & empty) (empty))

let test_partial () =
  Alcotest.check Alcotest.bool "is subset" false Set.(subset (1 & 2 & empty) (1 & 3 & empty))

let test_disjoint () =
  Alcotest.check Alcotest.bool "is subset" false Set.(subset (1 & 2 & empty) (3 & 4 & empty))

let () =
  Alcotest.run "subset" [
    "subset", [
      Alcotest.test_case "normal" `Quick test_normal;
      Alcotest.test_case "left_empty" `Quick test_left_empty;
      Alcotest.test_case "right_empty" `Quick test_right_empty;
      Alcotest.test_case "partial" `Quick test_partial;
      Alcotest.test_case "disjoint" `Quick test_disjoint;
    ]
  ]
