module Stadium.Type.Action where

import Stadium.Type.ActionType (ActionType)
import Stadium.Type.Targets (Targets)

--------------------------------------------------------------------------------
-- Action
--------------------------------------------------------------------------------

data Action

foreign import data MkAction :: ActionType -> Targets -> Action

--------------------------------------------------------------------------------
