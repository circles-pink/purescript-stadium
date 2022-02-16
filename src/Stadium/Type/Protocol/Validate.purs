module Stadium.Type.Protocol.Validate where

import Prelude
import Prim.Boolean (True)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Stadium.Type.Either (class First, Either, Right, Left)
import Stadium.Type.Protocol.Error (Error)
import Stadium.Type.Protocol.Error (ErrInvalidTargetState)
import Stadium.Type.Protocol.GetKeys (class GetKeys)
import Stadium.Type.Protocol.Type (Protocol, Protocol', State, State', Action, Action', type (<:))
import Type.Data.Boolean (class If)
import Type.Data.List (class IsMember, Cons', List', Nil')
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- class Validate
--------------------------------------------------------------------------------
instance validate ::
  ( ValidateStates (Protocol r) rl o
  , RowToList r rl
  ) =>
  Validate (Protocol r) o

class Validate :: Protocol' -> Either Error Boolean -> Constraint
class Validate ptc o | ptc -> o

--------------------------------------------------------------------------------
-- class ValidateStates
--------------------------------------------------------------------------------
instance validateStatesNil :: ValidateStates ptc Nil (Right True)

instance validateStatesCons ::
  ( ValidateStates ptc tail o1
  , ValidateActions ptc ac' o2
  , RowToList ac ac'
  , First o1 o2 o
  ) =>
  ValidateStates ptc (Cons s (State ac) tail) o

class ValidateStates :: Protocol' -> RowList State' -> Either Error Boolean -> Constraint
class ValidateStates ptc st o | ptc st -> o

--------------------------------------------------------------------------------
-- class ValidateActions
--------------------------------------------------------------------------------
instance validateActionsNil :: ValidateActions ptc Nil (Right True)

instance validateActionsCons ::
  ( ValidateActions ptc tail o1
  , ValidateTargetStates ptc ac o2
  , First o1 o2 o
  ) =>
  ValidateActions ptc (Cons s (Action ac) tail) o

class ValidateActions :: Protocol' -> RowList Action' -> Either Error Boolean -> Constraint
class ValidateActions ptc ac o | ptc ac -> o

--------------------------------------------------------------------------------
-- class ValidateTargetStates
--------------------------------------------------------------------------------
instance validateTargetStatesNil :: ValidateTargetStates ptc Nil' (Right True)

instance validateTargetStatesCons ::
  ( ValidateTargetStates ptc tail o1
  , GetKeys ptc ks
  , IsMember s ks m
  , If m (Right True) (Left (ErrInvalidTargetState s)) o2
  , First o1 o2 o
  ) =>
  ValidateTargetStates ptc (Cons' s tail) o

class ValidateTargetStates :: Protocol' -> List' Symbol -> Either Error Boolean -> Constraint
class ValidateTargetStates ptc sts o | ptc sts -> o

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
testsValidate :: Unit
testsValidate =
  unit
    <> testValidate
        (Proxy :: _ (Protocol ()))
        (Proxy :: _ (Right True))
    <> testValidate
        ( Proxy ::
            _
              ( Protocol
                  ( state1 ::
                      State
                        ( action1 ::
                            Action
                              ( Nil'
                                  <: "state1"
                                  <: "stateX"
                                  <: "state2"
                              )
                        )
                  , state2 :: State ()
                  )
              )
        )
        (Proxy :: _ (Left (ErrInvalidTargetState "stateX")))
    <> testValidate
        ( Proxy ::
            _
              ( Protocol
                  ( state1 ::
                      State
                        ( action1 ::
                            Action
                              ( Nil'
                                  <: "state1"
                                  <: "state2"
                              )
                        )
                  , state2 :: State ()
                  )
              )
        )
        (Proxy :: _ (Right True))
  where
  testValidate :: forall a b. Validate a b => Proxy a -> Proxy b -> Unit
  testValidate _ _ = unit
