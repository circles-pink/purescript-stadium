module Stadium.Type.Protocol.Error where

import Prim.TypeError (Text)
import Stadium.Type.ErrorMsg (class ToErrorMsg, type (:|:), Msg, TickText)

data Error

foreign import data ErrInvalidTargetState :: Symbol -> Error

--------------------------------------------------------------------------------
-- class ToDoc
--------------------------------------------------------------------------------
instance toDocErrInvalidTargetState ::
  ToErrorMsg (ErrInvalidTargetState s) ( Msg
        (Text "target state " :|: TickText s :|: Text " does not exist")
    )
