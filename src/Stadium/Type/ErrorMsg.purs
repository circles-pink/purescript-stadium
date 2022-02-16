module Stadium.Type.ErrorMsg where

import Prelude
import Prim.TypeError (class Fail, Above, Beside, Doc, Text)
import Stadium.Type.Either (Either, Left, Right)

class ToErrorMsg :: forall e. e -> ErrorMsg -> Constraint
class ToErrorMsg e m | e -> m

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data ErrorMsg

foreign import data Msg :: Doc -> ErrorMsg

foreign import data Scope :: Symbol -> ErrorMsg -> ErrorMsg

--------------------------------------------------------------------------------
-- class ToDoc
--------------------------------------------------------------------------------
instance toDocMsg :: ToDoc (Msg s) (Text "" :--: s)

instance toDocScope ::
  (ToDoc msg b) =>
  ToDoc (Scope s msg) ((Text "At " :|: Text s) :--: b)

class ToDoc :: ErrorMsg -> Doc -> Constraint
class ToDoc e m | e -> m

--------------------------------------------------------------------------------
-- class FailOnLeft
--------------------------------------------------------------------------------
instance failOnLeft ::
  ( Fail d
  , ToErrorMsg a e
  , ToDoc e d
  ) =>
  FailOnLeft (Left a)

instance failOnLeft' :: FailOnLeft (Right b)

class FailOnLeft :: forall a b. Either a b -> Constraint
class FailOnLeft e

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
infixr 6 type Beside as :|:

infixr 6 type Above as :--:

type NoText
  = Text ""

type TickText :: Symbol -> Doc
type TickText s
  = Text "`" :|: Text s :|: Text "`"
