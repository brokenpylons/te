open Te_bot
module T = Types

type test =
  {
    name: string;
    input: T.Symbol.t list;
    trace: Trace.t;
  }

module type CONTEXT = sig
  module R = Re.Abstract
  module Production: sig
    type t
    val lhs: t -> T.Labeled_var.t
    val rhs: t -> T.Symbols.t R.t
    val make: T.Labeled_var.t -> T.Symbols.t R.t -> t
  end

  module Test: sig
    type t = test =
      {
        name: string;
        input: T.Symbol.t list;
        trace: Trace.t;
      }
    val code: string -> T.Symbol.t 
    val eof: T.Symbol.t
    val vertex: T.State.t -> int -> T.Vertex.t
  end
  val variables: (string, 'a) Vector.t -> (T.Var.t, 'a) Vector.t
  val var: T.Var.t -> T.Symbols.t R.t
  val codes: string -> T.Symbols.t R.t
  val eof: T.Symbols.t R.t
end

module type SPEC' = functor (Context: CONTEXT) -> sig
  open Context

  val start: T.Var.t
  val syntactic: T.Var.t list
  val lexical: T.Var.t list
  val parser: Production.t list
  val scanner: Production.t list
end

module type SPEC = functor (Context: CONTEXT) -> sig
  open Context

  val start: T.Var.t
  val syntactic: T.Var.t list
  val lexical: T.Var.t list
  val parser: Production.t list
  val scanner: Production.t list
  val tests: Test.t list
end

module Context: CONTEXT = struct
  module R = Re.Abstract
  module Production = struct
    type t = T.Labeled_var.t * T.Symbols.t R.t
    let make lhs rhs = (lhs, rhs)
    let lhs = fst
    let rhs = snd
  end
  module Test = struct
    type t = test =
      {
        name: string;
        input: T.Symbol.t list;
        trace: Trace.t;
      }
    let code s = T.Symbol.Code (T.Codes.the @@ T.Codes.of_string s)
    let eof = T.Symbol.Eof
    let vertex s pos =
      T.Vertex.make s pos
  end
  let variables vs = T.Var.make ~supply:T.Var.supply vs
  let var x = R.lits (T.Symbols.of_vars (T.Vars.singleton x))
  let codes x = R.lits (T.Symbols.of_codes (T.Codes.of_string x))
  let eof = R.lits T.Symbols.eof
end

module Build(Spec: SPEC') = struct
  module Spec' = Spec(Context)
  module X = Gsglr.Make(Tables.Unoptimized)

  let convert x =
    List.to_seq x
    |> Seq.map (fun p -> Tn.Production.{
        lhs = Context.Production.lhs p;
        rhs = Re.Abstract.map Tn.Lits.of_symbols (Context.Production.rhs p);
      })

  let driver () =
    let lexical = T.Vars.of_list Spec'.lexical in
    let (first, lookahead, g, b) = Tn.build lexical Spec'.start (Seq.append (convert Spec'.parser)  (convert Spec'.scanner)) in
    let t = Tables.Unoptimized.make lexical first lookahead g b in
    new X.driver t

  module Run = struct
    let code s = T.Symbol.Code (T.Codes.the @@ T.Codes.of_string s)
    let eof = T.Symbol.Eof
  end
end

module Test(Spec: SPEC) = struct
  module Spec' = Spec(Context)
  module X = Gsglr.Make(Tables.Unoptimized)

  let tests = Spec'.tests

  let convert x =
    List.to_seq x
    |> Seq.map (fun p -> Tn.Production.{
        lhs = Context.Production.lhs p;
        rhs = Re.Abstract.map Tn.Lits.of_symbols (Context.Production.rhs p);
      })

  let driver () =
    let lexical = T.Vars.of_list Spec'.lexical in
    let (first, lookahead, g, b) = Tn.build lexical Spec'.start (Seq.append (convert Spec'.parser)  (convert Spec'.scanner)) in
    let t = Tables.Unoptimized.make lexical first lookahead g b in
    new X.driver t
end

