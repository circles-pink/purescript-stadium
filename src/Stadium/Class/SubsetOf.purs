module Stadium.Class.SubsetOf where

import Prelude
import Prim.Boolean (True)
import Stadium.Type.Either (class First, Either, Left, Right)
import Type.Data.Boolean (class If)
import Type.Data.List (class IsMember, Cons', List', Nil', type (:>))
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- class SubsetOf
--------------------------------------------------------------------------------
instance subsetOfNil :: SubsetOf Nil' bs (Right True)

instance subsetOfCons ::
  ( SubsetOf as bs o1
  , IsMember a bs mem
  , If mem (Right True) (Left a) o2
  , First o1 o2 o
  ) =>
  SubsetOf (Cons' a as) bs o

class SubsetOf :: List' Symbol -> List' Symbol -> Either Symbol Boolean -> Constraint
class SubsetOf as bs o | as bs -> o

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
testsSubsetOf :: Unit
testsSubsetOf =
  unit
    <> testSubsetOf
        (Proxy :: _ (Nil'))
        (Proxy :: _ (Nil'))
        (Proxy :: _ (Right True))
    <> testSubsetOf
        (Proxy :: _ Nil')
        (Proxy :: _ ("a" :> Nil'))
        (Proxy :: _ (Right True))
    <> testSubsetOf
        (Proxy :: _ ("a" :> "b" :> Nil'))
        (Proxy :: _ ("a" :> "b" :> "c" :> Nil'))
        (Proxy :: _ (Right True))
    <> testSubsetOf
        (Proxy :: _ ("a" :> "b" :> "c" :> Nil'))
        (Proxy :: _ ("a" :> "b" :> Nil'))
        (Proxy :: _ (Left "c"))
  where
  testSubsetOf :: forall a b c. SubsetOf a b c => Proxy a -> Proxy b -> Proxy c -> Unit
  testSubsetOf _ _ _ = unit
