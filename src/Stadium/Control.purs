module Stadium.Control where

import Prelude
import Data.Array (fold)
import Data.Variant (Variant)
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
import Type.Data.List (Nil')
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

type ControlSpec st ac m
  = Unit

type Control st ac m
  = (st -> m Unit) -> ac -> st -> m Unit

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
  , Cons s ((tgSt -> m Unit) -> stData -> acData -> m Unit) r' r
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
  mkControl _ _ = undefined

class MkControl :: STM.StateMachine' -> Type -> Type -> Constraint
class MkControl stm ctlS ctl | stm -> ctl ctlS where
  mkControl :: Proxy stm -> ctlS -> ctl

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
data S1

data S2

data S1_A1

type MyState
  = Variant ( state1 :: S1 )

type MyAction
  = Variant
      ( state1 ::
          Variant
            ( action1 :: S1_A1
            )
      )

type MyProtocol
  = P.Protocol
      ( state1 ::
          P.State
            ( action1 :: P.Action Nil'
            )
      )

type MyStateMachine
  = STM.StateMachine MyProtocol MyState MyAction

tests :: Unit
tests =
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
                        { action1 :: (Variant () -> Effect Unit) -> S1 -> S1_A1 -> Effect Unit
                        }
                    }
              )
          ]
    ]
