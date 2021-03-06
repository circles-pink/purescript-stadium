module Stadium.Control
  ( Control
  , class GenActions
  , class GenControlSpec
  , class GenStates
  , class MkControl
  , mkControl
  , tests
  , toStateT
  ) where

import Prelude
import Control.Monad.State (State, StateT(..), execState, get, modify_, put)
import Data.Array (fold)
import Data.Identity (Identity(..))
import Data.Variant (Variant, inj)
import Effect (Effect)
import Prim.Boolean (True)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.TypeError (class Warn, class Fail, Doc, Text)
import Stadium.Type.Either (Either, Left, Right)
import Stadium.Type.ErrorMsg (class FailOnLeft, class ToDoc, class ToErrorMsg)
import Stadium.Type.Protocol as P
import Stadium.Type.State as S
import Stadium.Type.StateMachine (StateMachine, StateMachine')
import Stadium.Type.StateMachine as STM
import Stadium.Type.Tuple (Tuple)
import Test.Unit as T
import Test.Unit.Assert as A
import Type.Data.List (type (:>), Nil')
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

type Control st ac m
  = ((st -> st) -> m Unit) -> st -> ac -> m Unit

--------------------------------------------------------------------------------
-- class GenControlSpec
--------------------------------------------------------------------------------
instance genControlSpec ::
  ( GenStates m stm sts' r
  , RowToList sts sts'
  , TypeEquals stm (StateMachine (P.Protocol sts) st ac)
  , STM.Validate stm res
  , FailOnLeft res
  ) =>
  GenControlSpec m stm (Record r)

class GenControlSpec :: (Type -> Type) -> StateMachine' -> Type -> Constraint
class GenControlSpec m stm ctlS | stm -> ctlS

--------------------------------------------------------------------------------
-- class GenStates
--------------------------------------------------------------------------------
instance genStatesNil :: GenStates m stm Nil ()

instance genStatesCons ::
  ( GenStates m stm tail r'
  , Cons s (Record a) r' r
  , RowToList ac ac'
  , GenActions m stm s ac' a
  ) =>
  GenStates m stm (Cons s (P.State ac) tail) r

class GenStates :: (Type -> Type) -> StateMachine' -> RowList P.State' -> Row Type -> Constraint
class GenStates m stm sts ctlS | sts -> ctlS

--------------------------------------------------------------------------------
-- class GenActions
--------------------------------------------------------------------------------
instance genActionsNil :: GenActions m stm sn Nil ()

instance genActionsCons ::
  ( GenActions m stm sn tail r'
  , Cons s (((stData -> tgSt) -> m Unit) -> stData -> acData -> m Unit) r' r
  , STM.GetStateData stm sn stData
  , STM.GetActionData stm sn s acData
  , STM.GetTargetState stm sn s tgSt
  ) =>
  GenActions m stm sn (Cons s t tail) r

class GenActions :: (Type -> Type) -> StateMachine' -> P.StateName -> RowList P.Action' -> Row Type -> Constraint
class GenActions m stm sn acs ctlS | acs -> ctlS

--------------------------------------------------------------------------------
-- class MkControl
--------------------------------------------------------------------------------
instance mkControl' ::
  ( TypeEquals o (Control st ac m)
  , STM.Validate stm r
  , FailOnLeft r
  , TypeEquals stm (StateMachine ptc st ac)
  , GenControlSpec m stm ctlS
  ) =>
  MkControl stm ctlS o where
  mkControl _ = mkControlImpl

foreign import mkControlImpl :: forall a. a

class MkControl :: STM.StateMachine' -> Type -> Type -> Constraint
class MkControl stm ctlS ctl | stm -> ctl ctlS where
  mkControl :: Proxy stm -> ctlS -> ctl

--------------------------------------------------------------------------------
-- StateMonad
--------------------------------------------------------------------------------
toStateT :: forall st ac m. Monad m => Control st ac (StateT st m) -> ac -> StateT st m Unit
toStateT ctl ac = do
  st <- get
  ctl modify_ st ac

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
type MyState
  = Variant ( state1 :: Int )

myInit :: MyState
myInit = inj (Proxy :: _ "state1") 0

type MyAction
  = Variant
      ( state1 ::
          Variant
            ( action1 :: Int
            )
      )

type MyProtocol
  = P.Protocol
      ( state1 ::
          P.State
            ( action1 :: P.Action ("state1" :> Nil')
            )
      )

type MyStateMachine
  = STM.StateMachine MyProtocol MyState MyAction

_state1 = inj (Proxy :: _ "state1")

_state1_action1 = inj (Proxy :: _ "state1") <<< inj (Proxy :: _ "action1")

_state2 = inj (Proxy :: _ "state2")

tests' :: Unit
tests' =
  fold
    [ let
        test :: forall a1 a2 a3. GenControlSpec a1 a2 a3 => Proxy a1 -> Proxy a2 -> Proxy a3 -> Unit
        test _ _ _ = unit
      in
        fold
          [ test
              (Proxy :: _ Effect)
              (Proxy :: _ MyStateMachine)
              ( Proxy ::
                  _
                    { state1 ::
                        { action1 ::
                            ((Int -> Variant ( state1 :: Int )) -> Effect Unit) ->
                            Int ->
                            Int ->
                            Effect Unit
                        }
                    }
              )
          ]
    ]

tests :: T.TestSuite
tests =
  T.suite "Stadium.Control" do
    T.suite "mkControl" do
      T.test "setState const" do
        let
          myControl :: forall m. Monad m => ((MyState -> MyState) -> m Unit) -> MyState -> MyAction -> m Unit
          myControl =
            mkControl (Proxy :: _ MyStateMachine)
              { state1:
                  { action1:
                      \setState s a -> setState (\_ -> _state1 (s + a))
                  }
              }

          myAct :: MyAction -> StateT MyState Identity Unit
          myAct = toStateT myControl

          myStMonad = do
            myAct $ _state1_action1 10
            myAct $ _state1_action1 7
            myAct $ _state1_action1 4
        A.equal (_state1 21) (execState myStMonad myInit)
      T.test "setState as function" do
        let
          myControl :: forall m. Monad m => ((MyState -> MyState) -> m Unit) -> MyState -> MyAction -> m Unit
          myControl =
            mkControl (Proxy :: _ MyStateMachine)
              { state1:
                  { action1:
                      \setState _ a -> setState (\s -> _state1 (s + a))
                  }
              }

          myAct :: MyAction -> StateT MyState Identity Unit
          myAct = toStateT myControl

          myStMonad = do
            myAct $ _state1_action1 10
            myAct $ _state1_action1 7
            myAct $ _state1_action1 4
        A.equal (_state1 21) (execState myStMonad myInit)
