module Stadium.Type.Protocol.Ops where

import Prelude
import Data.Foldable (fold)
import Prim.Row (class Cons)
import Stadium.Type.Either (Either)
import Stadium.Type.Protocol.Type (Action', Protocol, Protocol', State, State', Action)
import Type.Data.List (type (:>), Nil')
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- class GetState
--------------------------------------------------------------------------------
instance getState :: (Cons sN st trash ptc) => GetState (Protocol ptc) sN st

class GetState :: Protocol' -> Symbol -> State' -> Constraint
class GetState ptc sN st | ptc sN -> st

--------------------------------------------------------------------------------
-- class GetAction
--------------------------------------------------------------------------------
instance getAction :: GetAction (Protocol ptc) sN aN at

class GetAction :: Protocol' -> Symbol -> Symbol -> Action' -> Constraint
class GetAction ptc sN aN ac | ptc sN aN -> ac

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
type State1
  = State ( action1 :: Action1 )

type Action1
  = Action Nil'

tests :: Unit
tests =
  fold
    [ fold
        let
          testGetState :: forall a b c. GetState a b c => Proxy a -> Proxy b -> Proxy c -> Unit
          testGetState _ _ _ = unit
        in
          [ testGetState
              (Proxy :: _ (Protocol ( state1 :: State1 )))
              (Proxy :: _ "state1")
              (Proxy :: _ State1)
          ]
    , fold
        let
          testGetAction :: forall a b c d. GetAction a b c d => Proxy a -> Proxy b -> Proxy c -> Proxy d -> Unit
          testGetAction _ _ _ _ = unit
        in
          [ testGetAction
              (Proxy :: _ (Protocol ( state1 :: State1 )))
              (Proxy :: _ "state1")
              (Proxy :: _ "action1")
              (Proxy :: _ Action1)
          ]
    ]
