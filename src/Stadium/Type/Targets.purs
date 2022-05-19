module Stadium.Type.Targets where

import Stadium.Type.StateName (StateName)
import Type.Data.List (List')

--------------------------------------------------------------------------------
-- Targets
--------------------------------------------------------------------------------

data Targets

foreign import data MkTargets :: List' StateName -> Targets
