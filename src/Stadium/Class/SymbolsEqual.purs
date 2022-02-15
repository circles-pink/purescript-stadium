module Stadium.Class.SymbolsEqual where

import Prelude
import Prim.Boolean (False, True)
import Stadium.Class.All (class All)
import Type.Data.Boolean (class And)
import Type.Data.List (Cons', List', Nil', type (:>))
import Type.Data.Symbol (class Equals)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- class SymbolsEqual
--------------------------------------------------------------------------------
instance symbolsEqual'Nil0 :: SymbolsEqual Nil' Nil' True
else instance symbolsEqual'Nil1 :: SymbolsEqual Nil' b False
else instance symbolsEqual'Nil2 :: SymbolsEqual a Nil' False

instance symbolsEqual'Cons ::
  ( SymbolsEqual xs1 xs2 r1
  , Equals x1 x2 r2
  , And r1 r2 o
  ) =>
  SymbolsEqual (Cons' x1 xs1) (Cons' x2 xs2) o

class SymbolsEqual :: List' Symbol -> List' Symbol -> Boolean -> Constraint
class SymbolsEqual s1 s2 out | s1 s2 -> out

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
test :: forall a b c. SymbolsEqual a b c => Proxy a -> Proxy b -> Proxy c -> Unit
test _ _ _ = unit

tests :: Unit
tests =
  unit
    <> test
        (Proxy :: _ Nil')
        (Proxy :: _ Nil')
        (Proxy :: _ True)
    <> test
        (Proxy :: _ ("a" :> "b" :> Nil'))
        (Proxy :: _ ("a" :> "b" :> Nil'))
        (Proxy :: _ True)
    <> test
        (Proxy :: _ ("a" :> Nil'))
        (Proxy :: _ Nil')
        (Proxy :: _ False)
    <> test
        (Proxy :: _ ("a" :> Nil'))
        (Proxy :: _ ("b" :> Nil'))
        (Proxy :: _ False)
