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
    val vertex: T.State.t list -> int -> T.Vertex.t
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
    let vertex ss pos =
      T.Vertex.make (T.States.of_list ss) pos
  end
  let variables vs = T.Var.make ~supply:T.Var.supply vs
  let var x = R.lits (T.Symbols.of_vars (T.Vars.singleton x))
  let codes x = R.lits (T.Symbols.of_codes (T.Codes.of_string x))
  let eof = R.lits T.Symbols.eof
end

module Build(Spec: SPEC') = struct
  module Spec' = Spec(Context)
  module X = Gsglr.Make(Tables.V1.Unoptimized)

  let convert x =
    List.to_seq x
    |> Seq.map (fun p -> Context.Production.(lhs p, Re.Abstract.map Tn.Lits.of_symbols (rhs p)))

  let driver () =
    let tokens = T.Vars.of_list Spec'.lexical in
    let (b, g) = Tn.Builder.make' ~tokens Spec'.start (convert Spec'.parser) (convert Spec'.scanner) in
    let t = Tables.V1.Unoptimized.make ~tokens g b in
    new X.driver t

  module Run = struct
    let code s = T.Symbol.Code (T.Codes.the @@ T.Codes.of_string s)
    let eof = T.Symbol.Eof
  end
end

module Build_new(Spec: SPEC') = struct
  module Spec' = Spec(Context)
  module X = Gsglr.Make(Tables.V1.Unoptimized)

  let convert x =
    List.to_seq x
    |> Seq.map (fun p -> Tn2.Production.{
        lhs = Context.Production.lhs p;
        rhs = Re.Abstract.map Tn2.Lits.of_symbols (Context.Production.rhs p);
      })

  let build () =
    let tokens = T.Vars.of_list Spec'.lexical in

    let c = Tn2.construct ~supply:(T.State.fresh_supply ()) Spec'.start tokens (Tn2.index_productions (Seq.append (convert Spec'.parser) (convert Spec'.scanner))) in
    let d = Tn2.collapse ~supply:(T.State.fresh_supply ()) c in
    Fmt.pr "%s@," (Dot.string_of_graph (Tn2.to_dot'''' d));

    let cprod = Tn2.index_collapsed_productions (Tn2.collapsed_productions d) in

    let p = Tn2.subset ~supply:(T.State.fresh_supply ()) d in
    Fmt.pr "%s@," (Dot.string_of_graph (Tn2.to_dot'''' p));

    (*Fmt.pr "%s"  (Dot.string_of_graph (Tn2.to_dot' p));*)
    (*let e = Tn2.enhance p d in
    Fmt.pr "%s@,"  (Dot.string_of_graph (Tn2.to_dot''' e));*)

    let ep = Tn2.enhanced_productions cprod p in

    (*Tn2.print_productions ep;*)

    Fmt.pr "ANALYSIS";
    let analysis = Tn2.Analysis.compute ep in

    Fmt.pr "LOOKBACK";
    let lookback = Tn2.Lookback.of_seq ep in

    Fmt.pr "LOOKAHEAD";
    let lookahead' = Tn2.Lookahead.compute analysis lookback tokens in

    Fmt.pr "%s@,"  (Dot.string_of_graph (Tn2.to_dot''''' (Tn2.with_lookahead lookahead' p)));

    Fmt.pr "%s@,"  (Dot.string_of_graph (Tn2.to_dot'''''' (Tn2.with_nullable lookahead' p)));


    Fmt.pr "@,NC@,";
    let nc = Tn2.noncannonical tokens lookahead' p in
    Fmt.pr "%s"  (Dot.string_of_graph (Tn2.to_dot'''' nc));

    let nc' = Tn2.erase_scan nc in

    let nc'' = Tn2.subset ~supply:(T.State.fresh_supply ()) nc' in

    Fmt.pr "@,NC@,";
    Fmt.pr "%s"  (Dot.string_of_graph (Tn2.to_dot'''' nc''));

    let ep = Tn2.enhanced_productions cprod nc'' in

    Fmt.pr "ANALYSIS";
    let analysis = Tn2.Analysis.compute ep in

    Fmt.pr "LOOKBACK";
    let lookback = Tn2.Lookback.of_seq ep in

    Fmt.pr "LOOKAHEAD";
    let lookahead' = Tn2.Lookahead.compute analysis lookback tokens in

    (*Tn2.print_productions ep;*)

    Fmt.pr "%s@,"  (Dot.string_of_graph (Tn2.to_dot''''' (Tn2.with_lookahead lookahead' nc'')));


    Fmt.pr "%s@,"  (Dot.string_of_graph (Tn2.to_dot'''''' (Tn2.with_nullable lookahead' nc')));

    ep

    (*Tn.Builder.make' ~tokens Spec'.start (convert Spec'.parser) (convert Spec'.scanner)*)

  module Run = struct
    let code s = T.Symbol.Code (T.Codes.the @@ T.Codes.of_string s)
    let eof = T.Symbol.Eof
  end
end

module Test(Spec: SPEC) = struct
  module Spec' = Spec(Context)
  module X = Gsglr.Make(Tables.V1.Unoptimized)

  let tests = Spec'.tests

  let convert x =
    List.to_seq x
    |> Seq.map (fun p -> Context.Production.(lhs p, Re.Abstract.map Tn.Lits.of_symbols (rhs p)))

  let driver () =
    let tokens = T.Vars.of_list Spec'.lexical in
    let (b, g) = Tn.Builder.make ~tokens Spec'.start (convert Spec'.parser) (convert Spec'.scanner) in
    let t = Tables.V1.Unoptimized.make ~tokens g b in
    new X.driver t
end

