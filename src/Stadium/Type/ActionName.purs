module Stadium.Type.ActionName where

import Prelude

--------------------------------------------------------------------------------
-- ActionName
--------------------------------------------------------------------------------

data ActionName

foreign import data MkActionName :: Symbol -> ActionName

--------------------------------------------------------------------------------
