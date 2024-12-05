open Te_bot
open Te_core
module T = Types

module New = Spec.Build(functor(Context: Spec.CONTEXT) -> struct
  open Context

  let Vector.[
      start;
      json;
      value;
      object_;
      array;
      string;
      number;
      true_;
      false_;
      null;
      members;
      member;
      elements;
      element;
      ws;

      u;
      object';
      array';
      string';
      number';
      true';
      false';
      null';
    ] = variables Vector.[
      "start";
      "json";
      "value";
      "object";
      "array";
      "string";
      "number";
      "true";
      "false";
      "null";
      "members";
      "member";
      "elements";
      "element";
      "ws";

      "_";
      "Object";
      "Array";
      "String";
      "Number";
      "True";
      "False";
      "Null";
    ]
  let syntactic = [
    json;
    value;
    object_;
    members;
    member;
    array;
    elements;
    element;
  ]
  let lexical = [
    number;
    string;
    true_;
    false_;
    null;
  ]

  let start = start

  let parser =
    Production.[
      make (u, start) R.(var json * plus eof);

      make (object', value) (var object_);
      make (array', value) (var array);
      make (string', value) (var string);
      make (number', value) (var number);
      make (true', value) (var true_);
      make (false', value) (var false_);
      make (null', value) (var null);

      make (u, object_) R.(codes "{" * var ws * var members * codes "}" * var ws);
      make (u, members) R.(star (var member * codes "," * var ws) * var member);
      make (u, member) R.(var string * var ws * codes ":" * var ws * codes ":" * var ws * var value);

      make (u, array) R.(codes "[" * var ws * var members * codes "]" * var ws);
      make (u, elements) R.(star (var element * codes "," * var ws) * var element);
      make (u, element) (var value);
    ]

  let scanner =
    let digit = range "0" "9" in
    let digits = R.(plus (range "0" "9")) in
    let fraction = R.(codes "." * digits) in
    let exponent = R.(codes "eE" * (opt (codes "+-")) * digits) in
    let integer = R.(opt (codes "-") * (digit + range "1" "9" * digits)) in

    let hex = R.(digit + range "A" "F" + range "a" "f") in
    let escape = R.(codes "\\/bfnrt" + codes "u" * hex * hex * hex * hex) in
    let characters = R.(diff (not_range "\u{0000}" "\u{0019}") (codes "\"\\") + codes "\\" * escape ) in
    Production.[
      make (u, number) R.(integer * opt fraction * opt exponent);
      make (u, string) R.(codes "\"" * star characters * codes "\"");
      make (u, true_) (text "true");
      make (u, false_) (text "false");
      make (u, null) (text "null");
    ]

end)

let _ =
  let d = New.driver () in
  New.Run.file (fun c -> d#read c) "test.json";
  Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot);
  Fmt.pr "@[%s@]" (Dot.string_of_graph (T.Node_packed_forest.to_dot d#forest))
