module Stadium.Type where

import Type.Map (Map)

--------------------------------------------------------------------------------
-- StateMachine
--------------------------------------------------------------------------------
data StateMachine

foreign import data MkStateMachine :: Map StateName State -> StateMachine

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

data State

foreign import data MkState :: StateType -> StateActions -> State

--------------------------------------------------------------------------------
-- StateName
--------------------------------------------------------------------------------

data StateName

foreign import data MkStateName :: Symbol -> StateName

--------------------------------------------------------------------------------
-- StateType
--------------------------------------------------------------------------------

data StateType

foreign import data MkStateType :: Type -> StateType

--------------------------------------------------------------------------------
-- StateActions
--------------------------------------------------------------------------------

data StateActions

foreign import data MkStateActions :: Map ActionName Action -> StateActions

--------------------------------------------------------------------------------
-- StateAction
--------------------------------------------------------------------------------

data Action

foreign import data MkAction :: ActionType -> Targets -> Action

--------------------------------------------------------------------------------
-- ActionName
--------------------------------------------------------------------------------

data ActionName

foreign import data MkActionName :: Symbol -> ActionName

--------------------------------------------------------------------------------
-- ActionType
--------------------------------------------------------------------------------

data ActionType

foreign import data MkActionType :: Type -> ActionType

--------------------------------------------------------------------------------
-- Targets
--------------------------------------------------------------------------------

data Targets

foreign import data MkTargets :: Targets

foreign import data AddTarget :: Target -> Targets -> Targets

--------------------------------------------------------------------------------
-- Target
--------------------------------------------------------------------------------

data Target

foreign import data MkTarget :: StateName -> Target

