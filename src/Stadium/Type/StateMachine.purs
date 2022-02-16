module Stadium.Type.StateMachine where

import Prelude
import Data.Foldable (fold)
import Data.Variant (Variant)
import Prim.Boolean (True)
import Prim.TypeError (Above, Text)
import Stadium.Class.All (class All)
import Stadium.Type.Action (Action)
import Stadium.Type.Action as A
import Stadium.Type.Either (class First3, class LMap, Either, Right)
import Stadium.Type.ErrorMsg (class FailOnLeft, class ToErrorMsg, Msg, Scope)
import Stadium.Type.Protocol (Protocol, Protocol')
import Stadium.Type.Protocol as P
import Stadium.Type.State (State)
import Stadium.Type.State as S
import Type.Data.List (type (:>), Nil')
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

data StateMachine'

foreign import data StateMachine :: Protocol' -> State -> Action -> StateMachine'

--------------------------------------------------------------------------------
-- type Error
--------------------------------------------------------------------------------
data Error

foreign import data ErrProtocol :: P.Error -> Error

foreign import data ErrState :: S.Error -> Error

foreign import data ErrAction :: A.Error -> Error

instance toErrorMsgErrProtocol :: (ToErrorMsg a b) => ToErrorMsg (ErrProtocol a) (Scope "Protocol type" b)

instance toErrorMsgErrState :: (ToErrorMsg a b) => ToErrorMsg (ErrState a) (Scope "State type" b)

instance toErrorMsgErrAction :: (ToErrorMsg a b) => ToErrorMsg (ErrAction a) (Scope "Action type" b)

--------------------------------------------------------------------------------
-- class Validate
--------------------------------------------------------------------------------
instance validate' ::
  ( P.Validate ptc o1
  , S.Validate ptc st o2
  , A.Validate ptc ac o3
  , LMap o1 ErrProtocol o1'
  , LMap o2 ErrState o2'
  , LMap o3 ErrAction o3'
  , First3 o1' o2' o3' o
  ) =>
  Validate (StateMachine ptc st ac) o

class Validate :: StateMachine' -> Either Error Boolean -> Constraint
class Validate stm o | stm -> o

validate ::
  forall stm ptc st ac r.
  TypeEquals stm (StateMachine ptc st ac) =>
  Validate stm r =>
  FailOnLeft r => Proxy stm -> Unit
validate _ = unit

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
tests :: Unit
tests =
  let
    test :: forall a b. Validate a b => Proxy a -> Proxy b -> Unit
    test _ _ = unit
  in
    fold
      [ test
          ( Proxy ::
              _
                ( StateMachine
                    (Protocol ())
                    (Variant ())
                    (Variant ())
                )
          )
          (Proxy :: _ (Right True))
      ]
