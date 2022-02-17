module Stadium.Type.StateMachine where

import Prelude
import Data.Foldable (fold)
import Data.Variant (Variant)
import Prim.Boolean (True)
import Prim.Row (class Cons)
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
import Stadium.Util (type ($))
import Type.Data.List (type (:>), Cons', List', Nil')
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data StateMachine'

foreign import data StateMachine :: Protocol' -> State -> Action -> StateMachine'

type StateName
  = Symbol

type ActionName
  = Symbol

--------------------------------------------------------------------------------
-- type Error
--------------------------------------------------------------------------------
data Error

foreign import data ErrProtocol :: P.Error -> Error

foreign import data ErrState :: S.Error -> Error

foreign import data ErrAction :: A.Error -> Error

instance toErrorMsgErrProtocol ::
  (ToErrorMsg a b) =>
  ToErrorMsg (ErrProtocol a) (Scope "StateMachine" $ Scope "Protocol type" b)

instance toErrorMsgErrState ::
  (ToErrorMsg a b) =>
  ToErrorMsg (ErrState a) (Scope "StateMachine" $ Scope "State type" b)

instance toErrorMsgErrAction ::
  (ToErrorMsg a b) =>
  ToErrorMsg (ErrAction a) (Scope "StateMachine" $ Scope "Action type" b)

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
-- class GetStateData
--------------------------------------------------------------------------------
instance getStateData ::
  (Cons sn t trash st) =>
  GetStateData (StateMachine _ptc (Variant st) _ac) sn t

class GetStateData :: StateMachine' -> StateName -> Type -> Constraint
class GetStateData stm sn t | stm sn -> t

--------------------------------------------------------------------------------
-- class GetActionData
--------------------------------------------------------------------------------
instance getActionData ::
  ( Cons sn t trash ac
  , TypeEquals t (Variant v)
  , Cons an h trash' v
  ) =>
  GetActionData (StateMachine _ptc _st (Variant ac)) sn an h

class GetActionData :: StateMachine' -> StateName -> ActionName -> Type -> Constraint
class GetActionData stm sn an t | stm sn an -> t

--------------------------------------------------------------------------------
-- class GetTargetState
--------------------------------------------------------------------------------
instance getTargetState ::
  ( Cons sn (P.State t) trash v
  , Cons an (P.Action h) trash' t
  , GetTargetState' stm h r
  , TypeEquals stm (StateMachine (Protocol v) _st _ac)
  ) =>
  GetTargetState stm sn an (Variant r)

class GetTargetState :: StateMachine' -> StateName -> ActionName -> Type -> Constraint
class GetTargetState stm sn an t | stm sn an -> t

--------------------------------------------------------------------------------
-- class GetTargetState'
--------------------------------------------------------------------------------
instance getTargetState'Nil :: GetTargetState' stm Nil' ()

instance getTargetState'Cons ::
  ( GetTargetState' stm tail r'
  , GetStateData stm h stData
  , Cons h stData r' r
  ) =>
  GetTargetState' stm (Cons' h tail) r

class GetTargetState' :: StateMachine' -> List' StateName -> Row Type -> Constraint
class GetTargetState' stm sns t | stm sns -> t

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
data A

data B

data C

tests :: Unit
tests =
  fold
    [ let
        test :: forall a b. Validate a b => Proxy a -> Proxy b -> Unit
        test _ _ = unit
      in
        fold
          [ test
              ( Proxy ::
                  _
                    $ StateMachine
                      (Protocol ())
                      (Variant ())
                      (Variant ())
              )
              (Proxy :: _ (Right True))
          ]
    , let
        test :: forall a1 a2 a3. GetStateData a1 a2 a3 => Proxy a1 -> Proxy a2 -> Proxy a3 -> Unit
        test _ _ _ = unit
      in
        fold
          [ test
              ( Proxy ::
                  _
                    $ StateMachine
                      (Protocol ( state1 :: P.State () ))
                      (Variant ( state1 :: A ))
                      (Variant ())
              )
              (Proxy :: _ "state1")
              (Proxy :: _ A)
          ]
    , let
        test :: forall a1 a2 a3 a4. GetActionData a1 a2 a3 a4 => Proxy a1 -> Proxy a2 -> Proxy a3 -> Proxy a4 -> Unit
        test _ _ _ _ = unit
      in
        fold
          [ test
              ( Proxy ::
                  _
                    $ StateMachine
                      (Protocol ( state1 :: P.State ( action1 :: P.Action Nil' ) ))
                      (Variant ( state1 :: A ))
                      (Variant ( state1 :: Variant ( action1 :: B ) ))
              )
              (Proxy :: _ "state1")
              (Proxy :: _ "action1")
              (Proxy :: _ B)
          ]
    , let
        test :: forall a1 a2 a3 a4. GetTargetState a1 a2 a3 a4 => Proxy a1 -> Proxy a2 -> Proxy a3 -> Proxy a4 -> Unit
        test _ _ _ _ = unit
      in
        fold
          [ test
              ( Proxy ::
                  _
                    $ StateMachine
                      ( Protocol
                          ( state1 :: P.State ( action1 :: P.Action $ ("state1" :> "state3" :> Nil') )
                          , state1 :: P.State ()
                          )
                      )
                      (Variant ( state1 :: A, state2 :: B, state3 :: C ))
                      ( Variant
                          ( state1 :: Variant ( action1 :: B )
                          , state2 :: Variant ()
                          , state3 :: Variant ()
                          )
                      )
              )
              (Proxy :: _ "state1")
              (Proxy :: _ "action1")
              (Proxy :: _ $ Variant ( state1 :: A, state3 :: C ))
          ]
    ]
