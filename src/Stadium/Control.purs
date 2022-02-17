module Stadium.Control where

import Prelude
import Data.Array (fold)
import Data.Variant (Variant)
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
  ( GenControlSpec' sts' r
  , RowToList sts sts'
  , TypeEquals stm (StateMachine (P.Protocol sts) st ac)
  , STM.Validate stm res
  , FailOnLeft res
  ) =>
  GenControlSpec stm (Record r)

class GenControlSpec :: StateMachine' -> Type -> Constraint
class GenControlSpec stm ctlS | stm -> ctlS

--------------------------------------------------------------------------------
-- class GenControlSpec'
--------------------------------------------------------------------------------
instance genControlSpec'Nil :: GenControlSpec' Nil ()

instance genControlSpec'Cons ::
  ( GenControlSpec' tail r'
  , Cons s Unit r' r
  ) =>
  GenControlSpec' (Cons s t tail) r

class GenControlSpec' :: RowList P.State' -> Row Type -> Constraint
class GenControlSpec' sts ctlS | sts -> ctlS

--------------------------------------------------------------------------------
-- class MkControl
--------------------------------------------------------------------------------
instance mkControl' ::
  ( TypeEquals o (Control st ac m)
  , STM.Validate stm r
  , FailOnLeft r
  , TypeEquals stm (StateMachine ptc st ac)
  , GenControlSpec stm ctlS
  ) =>
  MkControl stm ctlS o where
  mkControl _ _ = undefined

class MkControl :: STM.StateMachine' -> Type -> Type -> Constraint
class MkControl stm ctlS ctl | stm -> ctl ctlS where
  mkControl :: Proxy stm -> ctlS -> ctl

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
type MyState
  = Variant ( state1 :: Int )

type MyAction
  = Variant ( state1 :: Variant () )

type MyProtocol
  = P.Protocol ( state1 :: P.State () )

type MyStateMachine
  = STM.StateMachine MyProtocol MyState MyAction

tests :: Unit
tests =
  fold
    [ let
        test :: forall a1 a2. GenControlSpec a1 a2 => Proxy a1 -> Proxy a2 -> Unit
        test _ _ = unit
      in
        fold
          [ test
              (Proxy :: _ MyStateMachine)
              ( Proxy ::
                  _
                    { state1 :: {}
                    }
              )
          ]
    ]
