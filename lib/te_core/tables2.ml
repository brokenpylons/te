open Te_bot
open! Prelude 
module T = Types
open Tn2

module Actions_multimap = struct
  include Lits_multimap(T.Actions)
  let pp = pp T.Actions.pp
end
module Goto_partial_map = struct
  include Lits_multimap(T.Statess)
  let pp = pp T.Statess.pp
end
module Back_map = struct
  include Multimap.Make1(T.State_to)(T.State_pair_partial)
  let pp = T.State_to.pp T.State_pair_partial.pp
end




