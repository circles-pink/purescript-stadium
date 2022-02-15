module Stadium where

import Prelude
import Data.Variant (Variant)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Type.Proxy (Proxy)
import Undefined (undefined)

-- instance getState :: GetState 
-- type Control st ac m
--   = (st -> m Unit) -> ac -> st -> m Unit
-- mkController ::
--   forall stm .
--   GetState ptc st => Proxy stm -> Control st ac m -> Control st ac m
-- mkController _ ctr = ctr
