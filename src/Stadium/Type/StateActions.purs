module Stadium.Type.StateActions where

import Stadium.Type.ActionName (ActionName)
import Stadium.Type.Action (Action)
import Type.Map (class Lookup, Map)

--------------------------------------------------------------------------------
-- StateActions
--------------------------------------------------------------------------------
data StateActions

foreign import data MkStateActions :: Map ActionName Action -> StateActions

--------------------------------------------------------------------------------

class GetAction :: StateActions -> ActionName -> Action -> Constraint
class GetAction stm sn s

instance etAction :: (Lookup m an a) => GetAction (MkStateActions m) an a

--------------------------------------------------------------------------------
