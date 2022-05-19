module Stadium.Type.Spec where

import Prelude

import Type.Data.List (List')

--------------------------------------------------------------------------------

data StateMachineSpec

foreign import data MkStateMachineSpec :: Row State -> StateMachineSpec

--------------------------------------------------------------------------------

data State

foreign import data MkState :: Type -> Row Action -> State

--------------------------------------------------------------------------------

data Action

foreign import data MkAction :: Type -> List' Symbol -> Action

--------------------------------------------------------------------------------