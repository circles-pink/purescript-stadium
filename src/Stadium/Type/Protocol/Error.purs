module Stadium.Type.Protocol.Error where

import Prelude
import Prim.Boolean (True)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.TypeError (class Fail, class Warn, Beside, Doc, Text)
import Stadium.Class.KeysOf (class KeysOf)
import Stadium.Type.Either (class First, Either, Right, Left)
import Type.Data.Boolean (class If)
import Type.Data.List (class IsMember, type (:>), Cons', List', Nil')
import Type.Proxy (Proxy(..))

data Error

foreign import data ErrInvalidTargetState :: Symbol -> Error

--------------------------------------------------------------------------------
-- class ToDoc
--------------------------------------------------------------------------------
instance toDocErrInvalidTargetState ::
  ToDoc (ErrInvalidTargetState s) ( Beside
        (Text "Invalid target state `")
        (Beside (Text s) (Text "`"))
    )

class ToDoc :: Error -> Doc -> Constraint
class ToDoc e doc | e -> doc
