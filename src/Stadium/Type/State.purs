module Stadium.Type.State where

import Prelude
import Data.Variant (Variant)
import Prim.Boolean (True)
import Prim.Row (class Cons)
import Stadium.Class.KeysOf (class KeysOf)
import Stadium.Class.SubsetOf (class SubsetOf)
import Stadium.Type.Either (class First, class LMap, Either, Left, Right)
import Stadium.Type.Protocol (Protocol, Protocol')
import Stadium.Type.Protocol as P
import Type.Data.List (List')
import Type.Proxy (Proxy(..))

type State
  = Type

--------------------------------------------------------------------------------
-- class GetSubState
--------------------------------------------------------------------------------
instance getSubState :: (Cons sym t trash r) => GetSubState (Variant r) sym t

class GetSubState :: State -> Symbol -> Type -> Constraint
class GetSubState st sym a | st sym -> a

--------------------------------------------------------------------------------
-- class GetKeys
--------------------------------------------------------------------------------
instance getKeys :: (KeysOf r ks) => GetKeys (Variant r) ks

class GetKeys :: State -> List' Symbol -> Constraint
class GetKeys st ks | st -> ks

--------------------------------------------------------------------------------
-- type Error
--------------------------------------------------------------------------------
data Error

foreign import data ErrMustBeVariant :: Error

foreign import data ErrMissingKey :: Symbol -> Error

foreign import data ErrExtraKey :: Symbol -> Error

--------------------------------------------------------------------------------
-- class Validate
--------------------------------------------------------------------------------
instance validate ::
  ( P.GetKeys ptc ksP
  , GetKeys (Variant v) ksS
  , SubsetOf ksP ksS r1
  , SubsetOf ksS ksP r2
  , LMap r1 ErrMissingKey o1
  , LMap r2 ErrExtraKey o2
  , First o1 o2 o
  ) =>
  Validate ptc (Variant v) o
else instance validate' :: Validate ptc st (Left ErrMustBeVariant)

class Validate :: Protocol' -> State -> Either Error Boolean -> Constraint
class Validate ptc st o | ptc st -> o

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
testsValidate :: Unit
testsValidate =
  unit
    <> testValidate
        (Proxy :: _ (Protocol ()))
        (Proxy :: _ Int)
        (Proxy :: _ (Left ErrMustBeVariant))
    <> testValidate
        (Proxy :: _ (Protocol ()))
        (Proxy :: _ (Variant ()))
        (Proxy :: _ (Right True))
    <> testValidate
        (Proxy :: _ (Protocol ( foo :: P.State () )))
        (Proxy :: _ (Variant ()))
        (Proxy :: _ (Left (ErrMissingKey "foo")))
    <> testValidate
        (Proxy :: _ (Protocol ()))
        (Proxy :: _ (Variant ( foo :: Int )))
        (Proxy :: _ (Left (ErrExtraKey "foo")))
  where
  testValidate :: forall a b c. Validate a b c => Proxy a -> Proxy b -> Proxy c -> Unit
  testValidate _ _ _ = unit
