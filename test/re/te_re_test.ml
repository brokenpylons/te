open Te_core
module T = Types

module R = Re.Abstract
module R' = Re.Concrete(T.Codes)


let test_to_seq () =
  let any = T.Codes.to_seq (T.Codes.of_string "01") in
  let num = R.lits (T.Codes.of_string "1") in
  let num0 = R.lits (T.Codes.of_string "0") in
  let dot = R.lits (T.Codes.of_string ".") in
  let dash = R.lits (T.Codes.of_string "-") in
  let times3 = R.(num * num * num) in
  let _pn = R.(times3 * (dot + star (dash * times3))) in
  let not = R.(comp (star num + star num0)) in
  let r = not in

  Fmt.pr "%a" T.Codes.pp (T.Codes.of_string "0123456789");
  Seq.iter (fun s ->
      Fmt.pr "%a@," (T.Code.pp) s)
    (T.Codes.to_seq @@ T.Codes.of_string "0123456789");

  Fmt.pr "@[<v>";
  Seq.iter (fun s ->
      Fmt.pr "%a@," (Fmt.seq ~sep:(Fmt.nop) T.Code.pp) s)
    (Seq.take 10 (R.to_seq ~cmp:T.Code.compare ~any T.Codes.to_seq r));
  Fmt.pr "@]";
  Alcotest.fail "dasd"
  (*Alcotest.check (module R') "equal" R.(lits (Ints.of_list [1; 2; 3]) * lits (Ints.of_list [1])) (R.lits (Ints.of_list [1]))*)

let () =
  Alcotest.run "re" [
    "to_seq", [
      Alcotest.test_case "test" `Quick test_to_seq;
    ]
  ]
