module Stadium.Type.Action where

import Prelude
import Data.Variant (Variant)
import Prim.Boolean (True)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, RowList, Cons, Nil)
import Stadium.Class.KeysOf (class KeysOf)
import Stadium.Class.SubsetOf (class SubsetOf)
import Stadium.Type.Either (class First, class LMap, Either, Left, Right)
import Stadium.Type.Protocol (Protocol, Protocol')
import Stadium.Type.Protocol as P
import Type.Data.List (List', Nil')
import Type.Proxy (Proxy(..))

type Action
  = Type

--------------------------------------------------------------------------------
-- class GetStateKeys
--------------------------------------------------------------------------------
instance getStateKeys :: (KeysOf r ks) => GetStateKeys (Variant r) ks

class GetStateKeys :: Action -> List' Symbol -> Constraint
class GetStateKeys st ks | st -> ks

--------------------------------------------------------------------------------
-- class GetAction
--------------------------------------------------------------------------------
-- class GetAction :: Action' -> Symbol -> Type -> Constraint
-- class GetAction st s ks | st -> ks
-- instance getAction ::
--   ( Cons s (Variant r') trash r
--     ) =>
--   GetActionKeys (Action (Variant r)) s r'
--------------------------------------------------------------------------------
-- type Error
--------------------------------------------------------------------------------
data Error

foreign import data ErrStateMustBeVariant :: Error

foreign import data ErrActionMustBeVariant :: Symbol -> Error

foreign import data ErrStateMissingKey :: Symbol -> Error

foreign import data ErrStateExtraKey :: Symbol -> Error

foreign import data ErrActionMissingKey :: Symbol -> Symbol -> Error

foreign import data ErrActionExtraKey :: Symbol -> Symbol -> Error

--------------------------------------------------------------------------------
-- class Validate
--------------------------------------------------------------------------------
instance validate ::
  ( ValidateState ptc (Variant v) o1
  , RowToList v vs
  , ValidateSubStates ptc vs o2
  , First o1 o2 o
  ) =>
  Validate ptc (Variant v) o
else instance validate' :: Validate ptc ac (Left ErrStateMustBeVariant)

class Validate :: Protocol' -> Action -> Either Error Boolean -> Constraint
class Validate ptc ac o | ptc ac -> o

--------------------------------------------------------------------------------
-- class ValidateState
--------------------------------------------------------------------------------
instance validateState ::
  ( P.GetKeys ptc ksP
  , GetStateKeys (Variant v) ksS
  , SubsetOf ksP ksS r1
  , SubsetOf ksS ksP r2
  , LMap r1 ErrStateMissingKey o1
  , LMap r2 ErrStateExtraKey o2
  , First o1 o2 o
  ) =>
  ValidateState ptc (Variant v) o
else instance validateState' :: ValidateState ptc ac (Left ErrStateMustBeVariant)

class ValidateState :: Protocol' -> Action -> Either Error Boolean -> Constraint
class ValidateState ptc ac o | ptc ac -> o

--------------------------------------------------------------------------------
-- class ValidateSubStates
--------------------------------------------------------------------------------
instance validateSubStatesNil :: ValidateSubStates ptc Nil (Right True)

instance validateSubStatesCons ::
  ( ValidateSubStates ptc tail o1
  , ValidateSubState ptc s v o2
  , First o1 o2 o
  ) =>
  ValidateSubStates ptc (Cons s (Variant v) tail) o
else instance validateSubStatesCons' :: ValidateSubStates ptc (Cons s a tail) (Left (ErrActionMustBeVariant s))

class ValidateSubStates :: Protocol' -> RowList Type -> Either Error Boolean -> Constraint
class ValidateSubStates ptc ac o | ptc ac -> o

--------------------------------------------------------------------------------
-- class ValidateSubState
--------------------------------------------------------------------------------
instance validateSubState ::
  ( KeysOf v ksA
  , P.GetState ptc s (P.State st)
  , KeysOf st ksP
  , SubsetOf ksP ksA r1
  , SubsetOf ksA ksP r2
  , LMap r1 (ErrActionMissingKey s) o1
  , LMap r2 (ErrActionExtraKey s) o2
  , First o1 o2 o
  ) =>
  ValidateSubState ptc s v o

class ValidateSubState :: Protocol' -> Symbol -> Row Type -> Either Error Boolean -> Constraint
class ValidateSubState ptc s ac o | ptc ac -> o

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
testsValidate :: Unit
testsValidate =
  unit
    <> testValidate
        (Proxy :: _ (Protocol ()))
        (Proxy :: _ Int)
        (Proxy :: _ (Left ErrStateMustBeVariant))
    <> testValidate
        (Proxy :: _ (Protocol ()))
        (Proxy :: _ (Variant ()))
        (Proxy :: _ (Right True))
    <> testValidate
        (Proxy :: _ (Protocol ( foo :: P.State () )))
        (Proxy :: _ (Variant ()))
        (Proxy :: _ (Left (ErrStateMissingKey "foo")))
    <> testValidate
        (Proxy :: _ (Protocol ()))
        (Proxy :: _ (Variant ( foo :: Int )))
        (Proxy :: _ (Left (ErrStateExtraKey "foo")))
    <> testValidate
        ( Proxy ::
            _ (P.Protocol ( state1 :: P.State ( action1 :: P.Action Nil' ) ))
        )
        ( Proxy ::
            _ (Variant ( state1 :: Int ))
        )
        (Proxy :: _ (Left (ErrActionMustBeVariant "state1")))
    <> testValidate
        ( Proxy ::
            _ (P.Protocol ( state1 :: P.State ( action1 :: P.Action Nil' ) ))
        )
        ( Proxy ::
            _ (Variant ( state1 :: Variant ( action1 :: Int ) ))
        )
        (Proxy :: _ (Right True))
    <> testValidate
        ( Proxy ::
            _ (P.Protocol ( state1 :: P.State ( action1 :: P.Action Nil', action2 :: P.Action Nil' ) ))
        )
        ( Proxy ::
            _ (Variant ( state1 :: Variant ( action1 :: Int ) ))
        )
        (Proxy :: _ (Left (ErrActionMissingKey "state1" "action2")))
    <> testValidate
        ( Proxy ::
            _ (P.Protocol ( state1 :: P.State ( action1 :: P.Action Nil' ) ))
        )
        ( Proxy ::
            _ (Variant ( state1 :: Variant ( action1 :: Int, action2 :: Int ) ))
        )
        (Proxy :: _ (Left (ErrActionExtraKey "state1" "action2")))
  where
  testValidate :: forall a b c. Validate a b c => Proxy a -> Proxy b -> Proxy c -> Unit
  testValidate _ _ _ = unit
