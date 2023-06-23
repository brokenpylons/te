open Te_bot
open! Prelude
open Te_core
module T = Types

module Tables = struct
  type symbol = Null | Code of T.Code.t | Var of T.Var.t

  type actions = {shift: bool; reduce: T.Reductions.t; null: T.Labeled_vars.t; matches: T.Labeled_vars.t; predictions: T.Labeled_vars.t}
  type t = {start: T.State.t; goto: T.State.t -> symbol -> T.State.t option; actions: T.State.t -> symbol -> actions}


  let start t = t.start

  let actions t s x =
    t.actions s x

  let goto t s x =
    t.goto s x

  let shift a =
    a.shift

  let reduce a =
    a.reduce

  let null a =
    a.null

  let matches a =
    a.matches

  let predictions a =
    a.predictions
end

module X = Gsglr.Make(Tables)

let cs = T.Codes.of_string
let c s = Tables.Code (T.Codes.the @@ T.Codes.of_string s)

let () =
  let Vector.[a; b; n] = T.Var.make ~supply:T.Var.supply Vector.["A"; "B"; "_"] in
  let t =
    Tables.{
      start = 1;
      goto = (fun s x ->
          (match s, x with
           | 1, Null -> Some 2
           | 2, (Code x) when T.Codes.mem x (cs "y") -> Some 3
           | 1, (Var x) when T.Var.equal x a -> Some 4
           | 4, Null -> Some 5
           | 5, (Code x) when T.Codes.mem x (cs "x") -> Some 6
           | 6, (Code x) when T.Codes.mem x (cs "x") -> Some 7
           (*| 4, (Var x) when T.Var.equal x a -> Some 8*)
           | 1, (Var x) when T.Var.equal x b -> Some 9
           | 9, (Var x) when T.Var.equal x b -> Some 10
           | 9, (Var x) when T.Var.equal x a -> Some 11
           | _, _ -> None));
      actions = (fun s x ->
          (match s, x with
           | 1, (Var x) when T.Var.equal x a -> {
               shift = true;
               null = T.Labeled_vars.empty;
               matches = T.Labeled_vars.empty;
               predictions = T.Labeled_vars.empty;
               reduce = T.Reductions.empty;
             }
           | 3, (Code x) when T.Codes.mem x (cs "x") -> {
               shift = false;
               null = T.Labeled_vars.empty;
               matches = T.Labeled_vars.of_list [
                   (n, a)
                 ];
               predictions = T.Labeled_vars.of_list [
                   (n, a)
                 ];
               reduce = T.Reductions.empty;
             }
           | 6, (Code x) when T.Codes.mem x (cs "x") -> {
               shift = false;
               null = T.Labeled_vars.empty;
               matches = T.Labeled_vars.empty;
               predictions = T.Labeled_vars.of_list [
                   (n, a)
                 ];
               reduce = T.Reductions.empty;
             }
           | 7, (Code x) when T.Codes.mem x (cs "x") -> {
               shift = false;
               null = T.Labeled_vars.empty;
               matches = T.Labeled_vars.of_list [
                   (n, a)
                 ];
               predictions = T.Labeled_vars.of_list [
                   (n, a)
                 ];
               reduce = T.Reductions.empty;
             }
           | 4, (Var x) when T.Var.equal x a -> {
               shift = true;
               null = T.Labeled_vars.empty;
               matches = T.Labeled_vars.empty;
               predictions = T.Labeled_vars.empty;
               reduce = T.Reductions.of_list [
                   T.Reduction.make (n, b) (Fixed 1);
                 ]
             }
           | 9, (Var x) when T.Var.equal x a -> {
               shift = true;
               null = T.Labeled_vars.of_list [
                   (n, b)
                 ];
               matches = T.Labeled_vars.empty;
               predictions = T.Labeled_vars.empty;
               reduce = T.Reductions.empty;
             }
           | _ -> {
               shift = false;
               null = T.Labeled_vars.empty;
               matches = T.Labeled_vars.empty;
               predictions = T.Labeled_vars.empty;
               reduce = T.Reductions.empty;
             }))
    }
  in
  let d = new X.driver t in
  d#read (c "y");
  d#read (c "x");
  d#read (c "x");
  d#read (c "x");
  d#read (c "x");
  Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot)



