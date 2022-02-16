module Stadium.Control where

import Prelude
import Data.Variant (Variant)
import Prim.Boolean (True)
import Prim.TypeError (class Warn, class Fail, Doc, Text)
import Stadium.Type.Either (Either, Left, Right)
import Stadium.Type.ErrorMsg (class FailOnLeft, class ToDoc, class ToErrorMsg)
import Stadium.Type.Protocol as P
import Stadium.Type.State as S
import Stadium.Type.StateMachine (StateMachine, StateMachine')
import Stadium.Type.StateMachine as STM
import Stadium.Type.Tuple (Tuple)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

type ControlSpec st ac m
  = Unit

type Control st ac m
  = (st -> m Unit) -> ac -> st -> m Unit

--------------------------------------------------------------------------------
-- class MkControl
--------------------------------------------------------------------------------
instance mkControl' ::
  ( TypeEquals stm (StateMachine ptc st ac)
  , STM.Validate stm r
  , FailOnLeft r
  , TypeEquals ctlS (ControlSpec st ac m)
  , TypeEquals o (Control st ac m)
  ) =>
  Foo stm ctlS o where
  mk _ _ = undefined

class MkControl :: STM.StateMachine' -> Type -> Type -> Constraint
class MkControl stm ctlS ctl | stm -> ctl ctlS where
  mk :: Proxy stm -> ctlS -> ctl

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
type MyState
  = Variant ()

type MyAction
  = Variant ()

type MyProtocol
  = P.Protocol ()

type MyMachine
  = STM.StateMachine MyProtocol MyState MyAction

myControl :: forall m. Control MyState MyAction m
myControl =
  mk
    (Proxy :: _ MyMachine)
    unit
