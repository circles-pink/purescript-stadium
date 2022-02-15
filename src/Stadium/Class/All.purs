module Stadium.Class.All where

import Prelude
import Prim.Boolean (False, True)
import Type.Data.Boolean (class And)
import Type.Data.List (Cons', List', Nil', type (:>))
import Type.Proxy (Proxy(..))

class All :: List' Boolean -> Boolean -> Constraint
class All bs o | bs -> o

instance allNil :: All Nil' True

instance allCons :: (All bs b', And b b' o) => All (Cons' b bs) o

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
test :: forall a b. All a b => Proxy a -> Proxy b -> Unit
test _ _ = unit

tests :: Unit
tests =
  unit
    <> test
        (Proxy :: _ (True :> True :> True :> Nil'))
        (Proxy :: _ True)
    <> test
        (Proxy :: _ (True :> True :> False :> Nil'))
        (Proxy :: _ False)
    <> test
        (Proxy :: _ Nil')
        (Proxy :: _ True)
