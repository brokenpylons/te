module Set = Te_bot.Comp_set.Make(struct
  include Te_bot.Balanced_binary_tree.Set.Size(Int)
  (*let pp = pp Fmt.int*)
end)

let test_subset_nn () =
  Alcotest.check Alcotest.bool "is subset" true Set.(subset (1 & 2 & empty) (1 & 2 & 3 & empty))

let test_subset_cc () =
  Alcotest.check Alcotest.bool "is subset" false Set.(subset (comp (1 & 2 & empty)) (comp (1 & 2 & 3 & empty)))

let test_subset_nc () =
  Alcotest.check Alcotest.bool "is subset" false Set.(subset (1 & 2 & empty) (comp (1 & 2 & 3 & empty)))

let test_subset_cn () =
  Alcotest.check Alcotest.bool "is subset" false Set.(subset (comp (1 & 2 & empty)) (1 & 2 & 3 & empty))

let () =
  Alcotest.run "comp_set" [
    "subset", [
      Alcotest.test_case "nn" `Quick test_subset_nn;
      Alcotest.test_case "cc" `Quick test_subset_cc;
      Alcotest.test_case "nc" `Quick test_subset_nc;
      Alcotest.test_case "cn" `Quick test_subset_cn;
    ]
  ]
