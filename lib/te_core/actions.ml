(*open Te_bot
open! Prelude 
module T = Types

type t =
  {
    accept: bool;
    read: bool;
    shift: T.Labeled_vars.t;
    reduce: T.Labeled_vars.t;
    null: T.Labeled_vars.t
  }
[@@deriving ord, eq, show]

let union x y =
  {
    accept = x.accept || y.accept;
    read = x.read || y.read;
    shift = T.Labeled_var.(x.shift <|> y.shift);
    reduce = T.Labeled_var.(x.reduce

    epsilon_shift = Typs.Return_set.union t1.epsilon_shift t1.epsilon_shift;
    reduce = Reduction_set.union t1.reduce t2.reduce;
    epsilon_reduce = Reduction_set.union t1.epsilon_reduce t2.epsilon_reduce;
  }

let empty = 
  {
    accept = false;
    shift = false;
    epsilon_shift = Typs.Return_set.empty;
    reduce = Reduction_set.empty;
    epsilon_reduce = Reduction_set.empty;
  }

let is_empty t =
  not t.shift && 
  Typs.Return_set.is_empty t.epsilon_shift &&
  Reduction_set.is_empty t.reduce &&
  Reduction_set.is_empty t.epsilon_reduce

let accept = 
  {
    accept = true;
    shift = true;
    epsilon_shift = Typs.Return_set.empty;
    reduce = Reduction_set.empty;
    epsilon_reduce = Reduction_set.empty;
  }

let shift = 
  {
    accept = false;
    shift = true;
    epsilon_shift = Typs.Return_set.empty;
    reduce = Reduction_set.empty;
    epsilon_reduce = Reduction_set.empty;
  }

let epsilon_shift x = 
  {
    accept = false;
    shift = false;
    epsilon_shift = Typs.Return_set.singleton x;
    reduce = Reduction_set.empty;
    epsilon_reduce = Reduction_set.empty;
  }

let reduce r =
  {
    accept = false;
    shift = false;
    epsilon_shift = Typs.Variable_set.empty;
    reduce = Reduction_set.singleton r;
    epsilon_reduce = Reduction_set.empty;
  }

let epsilon_reduce r =
  {
    accept = false;
    shift = false;
    epsilon_shift = Typs.Variable_set.empty;
    reduce = Reduction_set.empty;
    epsilon_reduce = Reduction_set.singleton r;
  }
  *)
