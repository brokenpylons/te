open Te_bot
module T = Types
open! Prelude

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
  val variable: T.Var.pre Supply.t -> string -> (T.Var.t * T.Var.pre Supply.t)
  val variable_supply: T.Var.pre Supply.t
  val var: T.Var.t -> T.Symbols.t R.t
  val codes: string -> T.Symbols.t R.t
  val eof: T.Symbols.t R.t

  (* Extended *)
  val not_codes: string -> T.Symbols.t R.t
  val text: string -> T.Symbols.t R.t
  val range: string -> string -> T.Symbols.t R.t
  val not_range: string -> string -> T.Symbols.t R.t
  val with_ws: T.Vars.t -> T.Symbols.t R.t -> Production.t list -> Production.t list
  val unextend: T.Var.pre Supply.t -> T.Var.t -> Production.t list -> Production.t list

  module Early_stop: sig
    val code: string -> T.Code.t
    val (=): T.Code.t -> T.Code.t -> bool
    val (<=): T.Code.t -> T.Code.t -> bool
  end
end

module type SPEC_TEST = functor (Context: CONTEXT) -> sig
  open Context

  val start: T.Var.t
  val syntactic: T.Var.t list
  val lexical: T.Var.t list
  val labels: T.Var.t list
  val parser: Production.t list
  val scanner: Production.t list
  val tests: Test.t list
end

module type SPEC = functor (Context: CONTEXT) -> sig
  open Context

  val start: T.Var.t
  val syntactic: T.Var.t list
  val lexical: T.Var.t list
  val labels: T.Var.t list
  val longest_match: T.Var.t list
  val parser: Production.t list
  val scanner: Production.t list
end

module type SPEC_CLASSIC = functor (Context: CONTEXT) -> sig
  open Context

  val start: T.Var.t
  val syntactic: T.Var.t list
  val lexical: T.Var.t list
  val labels: T.Var.t list
  val early_stop: (T.Var.t * (T.Symbol.t -> T.Symbol.t -> T.Symbol.t -> T.Symbol.t -> bool)) list
  val parser: Production.t list
  val scanner: Production.t list
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
  let variable supply v = T.Var.make' supply v
  let variable_supply = T.Var.supply
  let var x = R.lits (T.Symbols.of_vars (T.Vars.singleton x))
  let codes x = R.lits (T.Symbols.of_codes (T.Codes.of_string x))
  let eof = R.lits T.Symbols.eof

  (* Extended *)
  let not_codes x = R.lits (T.Symbols.of_codes (T.Codes.comp @@ T.Codes.of_string x))

  let text s =
    List.fold_right (fun c acc -> R.(lits (T.Symbols.of_codes @@ T.Codes.of_int c) * acc))
      (T.explode s) R.null

  let range_codes from to_ =
    let lf = List.the @@ T.explode from
    and lt = List.the @@ T.explode to_ in
    T.Codes.of_int_list @@ List.range lf lt

  let range from to_ =
    R.lits (T.Symbols.of_codes (range_codes from to_))

  let not_range from to_ =
    R.lits (T.Symbols.of_codes (T.Codes.comp (range_codes from to_)))

  let with_ws_re lexical ws r =
    R.flat_map (fun x ->
        let vars = T.Symbols.to_vars x in
        if not (T.Vars.disjoint lexical vars)
        then R.concat (R.lits x) ws
        else R.lits x)
      r

  let with_ws lexical ws =
    List.map (fun prod ->
        Production.make (Production.lhs prod) (with_ws_re lexical ws (Production.rhs prod)))

  let unextend_re supply lbl r =
    let r, loop_prods = R.unextend supply T.Var.synthetic
        (fun var -> T.Symbols.singleton (T.Symbol.Var var))
        r
    in
    r, List.map (fun (var, rhs) -> Production.make (lbl, var) rhs) loop_prods

  let unextend supply lbl prods =
    let prods, loop_prodss =
      prods
      |> List.fold_left_map (fun supply prod ->
          let supply, supply' = Supply.split2 supply in
          let rhs', loop_prods = unextend_re supply' lbl (Production.rhs prod) in
          (supply, (Production.make (Production.lhs prod) rhs', loop_prods)))
        supply
      |> List.split % snd
    in
    prods @ (List.flatten loop_prodss)

  module Early_stop = struct
    let code = T.Code.of_string
    let (=) = T.Code.equal
    let (<=) x y = T.Code.compare x y <=0
  end
end

module Build(Spec: SPEC) = struct
  module Spec' = Spec(Context)
  module X = Gsglr.Make(Tables.Optimized)

  let convert x =
    List.to_seq x
    |> Seq.map (fun p -> Tn.Production.{
        lhs = Context.Production.lhs p;
        rhs = Re.Abstract.map Tn.Lits.of_symbols (Context.Production.rhs p);
      })

  let tables ?(overexpand = false) () =
    let syntactic = T.Vars.of_list Spec'.syntactic in
    let lexical = T.Vars.of_list Spec'.lexical in
    let labels = T.Vars.of_list Spec'.labels in
    let longest_match = T.Vars.of_list Spec'.longest_match in

    let (lookahead', nullable', g, b) = Tn.build overexpand syntactic lexical labels longest_match Spec'.start (convert Spec'.parser) (convert Spec'.scanner) in
    Tables.Optimized.make Spec'.start lexical lookahead' nullable' g b

  let driver t =
    new X.driver t

  module Run = struct
    let code s = T.Symbol.Code (T.Codes.the @@ T.Codes.of_string s)
    let eof = T.Symbol.Eof

    let file f path =
      let d = Uutf.decoder (`Channel (open_in path)) in
      let rec loop () =
        match Uutf.decode d with
        | `Uchar u -> f (T.Symbol.Code (T.Code.of_int (Uchar.to_int u))); loop ()
        | `End -> f T.Symbol.Eof
        | _ -> assert false
      in loop ()
  end
end

module Classic(Spec: SPEC_CLASSIC) = struct
  module Spec' = Spec(Context)
  module X = Glr.Make(Tables.Optimized_classic)

  let convert x =
    List.map (fun p -> Tn.Production.{
        lhs = Context.Production.lhs p;
        rhs = Re.Abstract.map Tn.Lits.of_symbols (Context.Production.rhs p);
      }) x
    |> List.to_seq

  let tables () =
    let lexical = T.Vars.of_list Spec'.lexical in
    let syntactic = T.Vars.of_list Spec'.syntactic in
    let labels = T.Vars.of_list Spec'.labels in

    let (lookahead', nullable', gp, gs) = Tn.build_classic syntactic lexical labels Spec'.start (convert Spec'.parser) (convert Spec'.scanner) in
    Tables.Optimized_classic.make Spec'.start lexical lookahead' nullable' Spec'.early_stop gp gs

  let driver t =
    new X.driver t

  module Run = struct
    let code s = T.Symbol.Code (T.Codes.the @@ T.Codes.of_string s)
    let eof = T.Symbol.Eof

    let file f path =
      let d = Uutf.decoder (`Channel (open_in path)) in
      let rec loop () =
        match Uutf.decode d with
        | `Uchar u -> f (T.Symbol.Code (T.Code.of_int (Uchar.to_int u))); loop ()
        | `End -> 
          f T.Symbol.Eof;
          f T.Symbol.Eof;
          f T.Symbol.Eof;
          f T.Symbol.Eof;
          f T.Symbol.Eof
        | _ -> assert false
      in loop ()
  end
end


module Test(Spec: SPEC_TEST) = struct
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
    let syntactic = T.Vars.of_list Spec'.syntactic in
    let labels = T.Vars.of_list Spec'.labels in

    let (lookahead', nullable', g, b) = Tn.build false syntactic lexical labels T.Vars.empty Spec'.start (convert Spec'.parser) (convert Spec'.scanner) in
    let t = Tables.Unoptimized.make Spec'.start lexical lookahead' nullable' g b in
    new X.driver t
end

