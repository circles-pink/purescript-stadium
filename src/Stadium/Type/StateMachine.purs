module Stadium.Type.StateMachine where

import Prelude
import Data.Foldable (fold)
import Data.Variant (Variant)
import Prim.Boolean (True)
import Stadium.Class.All (class All)
import Stadium.Type.Action (Action)
import Stadium.Type.Action as A
import Stadium.Type.Either (class First3, class LMap, Either, Right)
import Stadium.Type.Protocol (Protocol, Protocol')
import Stadium.Type.Protocol as P
import Stadium.Type.State (State)
import Stadium.Type.State as S
import Type.Data.List (type (:>), Nil')
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

--------------------------------------------------------------------------------
-- class Validate
--------------------------------------------------------------------------------
instance validate ::
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
