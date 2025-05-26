open! Te_bot
open Te_core
open Te_top
module T = Types

module X = Spec.Build(functor(Context: Spec.CONTEXT) -> struct
  open Context

  let s = variable_supply
  let (start, s) = variable s "start"
  let (json, s) = variable s "json"
  let (value, s) = variable s "value"
  let (object_, s) = variable s "object_"
  let (array, s) = variable s "array"
  let (string, s) = variable s "string"
  let (number, s) = variable s "number"
  let (true_, s) = variable s "true_"
  let (false_, s) = variable s "false_"
  let (null_, s) = variable s "null_"
  let (members, s) = variable s "members"
  let (member, s) = variable s "member"
  let (elements, s) = variable s "elements"
  let (element, s) = variable s "element"
  let (lbrac, s) = variable s "lbrac"
  let (rbrac, s) = variable s "rbrac"
  let (lbrak, s) = variable s "lbrak"
  let (rbrak, s) = variable s "rbrak"
  let (comma, s) = variable s "comma"
  let (colon, s) = variable s "colon"
  let (ws, s) = variable s "ws"

  let (u, s) = variable s "u"
  let (object', s) = variable s "object'"
  let (array', s) = variable s "array'"
  let (string', s) = variable s "string'"
  let (number', s) = variable s "number'"
  let (true', s) = variable s "true'"
  let (false', s) = variable s "false'"
  let (null', s) = variable s "null'"

  let syntactic = [
    start;
    json;
    value;
    object_;
    members;
    member;
    array;
    elements;
    element;
    ws;
    number;
    string;
    true_;
    false_;
    null_;
    lbrac;
    rbrac;
    lbrak;
    rbrak;
    comma;
    colon;
  ]
  let lexical = []

  let lexical' = [
    ws;
    number;
    string;
    true_;
    false_;
    null_;
    lbrac;
    rbrac;
    lbrak;
    rbrak;
    comma;
    colon;
  ]

  let labels = [
    u;
    object';
    array';
    string';
    number';
    true';
    false';
    null';
  ]

  let longest_match = []

  let start = start

  let (s, _s') = Supply.split2 s

  let parser =
    (with_ws (T.Vars.of_list lexical') (var ws) @@ unextend s u (Production.[
      make (u, start) R.(var json * plus eof);
      make (u, json) R.(var ws * var value);

      make (object', value) (var object_);
      make (array', value) (var array);
      make (string', value) (var string);
      make (number', value) (var number);
      make (true', value) (var true_);
      make (false', value) (var false_);
      make (null', value) (var null_);

      make (u, object_) R.(var lbrac * opt (var members) * var rbrac);
      make (u, members) R.(star (var member * var comma) * var member);
      make (u, member) R.(var string * var colon * var value);

      make (u, array) R.(var lbrak * opt (var elements) * var rbrak);
      make (u, elements) R.(star (var element * var comma) * var element);
      make (u, element) (var value);
    ])) @
    let digit = range "0" "9" in
    let digits = R.(plus (range "0" "9")) in
    let fraction = R.(codes "." * digits) in
    let exponent = R.(codes "eE" * (opt (codes "+-")) * digits) in
    let integer = R.(opt (codes "-") * (digit + range "1" "9" * digits)) in

    let hex = R.(digit + range "A" "F" + range "a" "f") in
    let escape = R.(codes "\"\\/bfnrt" + codes "u" * hex * hex * hex * hex) in
    let characters = R.((not_codes "\"\\") + codes "\\" * escape) in
    (*unextend s' u*) Production.[
      make (u, number) R.(integer * opt fraction * opt exponent);
      make (u, string) R.(codes "\"" * star characters * codes "\"");
      make (u, true_) (text "true");
      make (u, false_) (text "false");
      make (u, null_) (text "null");
      make (u, lbrac) (codes "{");
      make (u, rbrac) (codes "}");
      make (u, lbrak) (codes "[");
      make (u, rbrak) (codes "]");
      make (u, comma) (codes ",");
      make (u, colon) (codes ":");
      make (u, ws) R.(star (codes " \n\r\t"));
    ]

    let scanner = []

end)

module B = Benchmark.Make(X)

let _ =
  (*let d = X.driver (X.tables ()) in
  let t = Sys.time() in
  X.Run.file (fun c -> d#read c) "linear/sample998998.json";
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  Fmt.pr "%b@." d#accept;*)
  (*Fmt.pr "@[%a@]" Trace.pp d#trace;*)
  (*Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot);*)
  (*Fmt.pr "@[%s@]" (Dot.string_of_graph (T.Node_packed_forest.to_dot d#forest))*)

  B.benchmark ()
