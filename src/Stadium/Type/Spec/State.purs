module Stadium.Type.Spec.State where

import Prelude

import Stadium.Type.Spec.Action (Action)
import Stadium.Type.State as SM

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

data State

foreign import data MkState :: Type -> Row Action -> State

--------------------------------------------------------------------------------

class StateFromSpec :: State -> SM.State -> Constraint
class StateFromSpec spec st

