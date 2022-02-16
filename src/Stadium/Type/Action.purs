module Stadium.Type.Action where

import Prelude
import Data.Variant (Variant)
import Prim.Boolean (True)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, RowList, Cons, Nil)
import Prim.TypeError (Text)
import Stadium.Class.KeysOf (class KeysOf)
import Stadium.Class.SubsetOf (class SubsetOf)
import Stadium.Type.Either (class First, class LMap, Either, Left, Right)
import Stadium.Type.ErrorMsg (class ToErrorMsg, type (:|:), Msg, Scope, TickText)
import Stadium.Type.Protocol (Protocol, Protocol')
import Stadium.Type.Protocol as P
import Stadium.Util (type ($), type (<<<))
import Type.Data.List (List', Nil')
import Type.Proxy (Proxy(..))

type Action
  = Type

type StateName
  = Symbol

type ActionName
  = Symbol

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

foreign import data ErrMustBeVariant :: Error

foreign import data ErrMissingKey :: Symbol -> Error

foreign import data ErrExtraKey :: Symbol -> Error

foreign import data ErrAction :: StateName -> Error -> Error

instance toDocErrMustBeVariant ::
  ToErrorMsg
    ErrMustBeVariant
    (Msg (Text "type must be a " :|: TickText "Variant" :|: Text " type."))

instance toDocErrMissingKey ::
  ToErrorMsg
    (ErrMissingKey s)
    (Msg (Text "Key " :|: TickText s :|: Text " must be defined."))

instance toDocErrExtraKey ::
  ToErrorMsg
    (ErrExtraKey s)
    (Msg (Text "Key " :|: TickText s :|: Text " should not be defined."))

instance toDocErrAction ::
  (ToErrorMsg e m) =>
  ToErrorMsg (ErrAction s e) (Scope s m)

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
else instance validate' :: Validate ptc ac (Left ErrMustBeVariant)

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
  , LMap r1 ErrMissingKey o1
  , LMap r2 ErrExtraKey o2
  , First o1 o2 o
  ) =>
  ValidateState ptc (Variant v) o
else instance validateState' :: ValidateState ptc ac (Left ErrMustBeVariant)

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
else instance validateSubStatesCons' :: ValidateSubStates ptc (Cons s a tail) (Left $ ErrAction s $ ErrMustBeVariant)

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
  , LMap r1 ErrMissingKey o1
  , LMap o1 (ErrAction s) o1'
  , LMap r2 ErrExtraKey o2
  , LMap o2 (ErrAction s) o2'
  , First o1' o2' o
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
        (Proxy :: _ $ Protocol ())
        (Proxy :: _ Int)
        (Proxy :: _ $ Left ErrMustBeVariant)
    <> testValidate
        (Proxy :: _ $ Protocol ())
        (Proxy :: _ $ Variant ())
        (Proxy :: _ $ Right True)
    <> testValidate
        (Proxy :: _ $ Protocol ( foo :: P.State () ))
        (Proxy :: _ $ Variant ())
        (Proxy :: _ $ Left (ErrMissingKey "foo"))
    <> testValidate
        (Proxy :: _ $ Protocol ())
        (Proxy :: _ $ Variant ( foo :: Int ))
        (Proxy :: _ $ Left $ ErrExtraKey "foo")
    <> testValidate
        ( Proxy ::
            _ $ P.Protocol ( state1 :: P.State ( action1 :: P.Action Nil' ) )
        )
        ( Proxy ::
            _ $ Variant ( state1 :: Int )
        )
        (Proxy :: _ $ Left $ ErrAction "state1" $ ErrMustBeVariant)
    <> testValidate
        ( Proxy ::
            _ $ P.Protocol ( state1 :: P.State ( action1 :: P.Action Nil' ) )
        )
        ( Proxy ::
            _ $ Variant ( state1 :: Variant ( action1 :: Int ) )
        )
        (Proxy :: _ (Right True))
    <> testValidate
        ( Proxy ::
            _ $ P.Protocol ( state1 :: P.State ( action1 :: P.Action Nil', action2 :: P.Action Nil' ) )
        )
        ( Proxy ::
            _ $ Variant ( state1 :: Variant ( action1 :: Int ) )
        )
        (Proxy :: _ $ Left $ ErrAction "state1" $ ErrMissingKey "action2")
    <> testValidate
        ( Proxy ::
            _ $ P.Protocol ( state1 :: P.State ( action1 :: P.Action Nil' ) )
        )
        ( Proxy ::
            _ $ Variant ( state1 :: Variant ( action1 :: Int, action2 :: Int ) )
        )
        (Proxy :: _ $ Left $ ErrAction "state1" $ ErrExtraKey "action2")
  where
  testValidate :: forall a b c. Validate a b c => Proxy a -> Proxy b -> Proxy c -> Unit
  testValidate _ _ _ = unit
