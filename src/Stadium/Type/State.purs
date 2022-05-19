module Stadium.Type.State where

import Prelude

import Stadium.Type.StateType (MkStateType, StateType)
import Stadium.Type.StateActions (MkStateActions, StateActions)
import Type.Map (Empty)

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------
data State

foreign import data MkState :: StateType -> StateActions -> State

type DefaultState = MkState (MkStateType Unit) (MkStateActions Empty)

--------------------------------------------------------------------------------

class GetStateType :: State -> StateType -> Constraint
class GetStateType st stt

instance getStateType :: GetStateType (MkState stt sta) stt

--------------------------------------------------------------------------------

class GetStateActions :: State -> StateActions -> Constraint
class GetStateActions st sa

instance getStateActions :: GetStateActions (MkState stt sta) sta

--------------------------------------------------------------------------------
