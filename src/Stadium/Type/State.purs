module Stadium.Type.State where

import Prelude

import Stadium.Type.StateActions (MkStateActions, StateActions)
import Stadium.Type.StateType (MkStateType, StateType)
import Type.Map (Empty)

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------
data State

foreign import data MkState :: StateType -> StateActions -> State

type DefaultState = MkState (MkStateType Unit) (MkStateActions Empty)

--------------------------------------------------------------------------------

class Mk :: StateType -> StateActions -> State -> Constraint
class Mk stt sta st | st -> stt sta, stt sta -> st

instance mk :: (GetStateType st stt, GetStateActions st sta) => Mk stt sta st

--------------------------------------------------------------------------------

class GetStateType :: State -> StateType -> Constraint
class GetStateType st stt

instance getStateType :: GetStateType (MkState stt sta) stt

--------------------------------------------------------------------------------

class GetStateActions :: State -> StateActions -> Constraint
class GetStateActions st sa

instance getStateActions :: GetStateActions (MkState stt sta) sta

--------------------------------------------------------------------------------
