module Stadium.Type.StateMachine
  ( MkStateMachine
  , StateMachine(..)
  , StateMap
  , class FromMap
  , class GetState
  , tests
  ) where

import Prelude

import Data.Foldable (fold)
import Prim.Row (class Cons)
import Stadium.Type.Action (Action)
import Stadium.Type.ActionName (ActionName)
import Stadium.Type.State (class GetStateType, MkState, State)
import Stadium.Type.State as S
import Stadium.Type.StateActions (MkStateActions)
import Stadium.Type.StateActions as SA
import Stadium.Type.StateName (class ToSym, MkStateName, StateName)
import Stadium.Type.StateType (class GetType, MkStateType)
import Stadium.Util (type ($), type (#))
import Type.Data.List (type (:>), Nil')
import Type.Map (class Lookup, Empty, Insert, Map)
import Type.Proxy (Proxy(..))
import Type.Row.Extra (class FilterRow)
import Type.Test (Test3, test3)

--------------------------------------------------------------------------------
-- StateMachine
--------------------------------------------------------------------------------

data StateMachine

type StateMap = Map StateName State

foreign import data MkStateMachine :: StateMap -> StateMachine

--------------------------------------------------------------------------------

class GetMap :: StateMachine -> StateMap -> Constraint
class GetMap stm m | stm -> m

instance getMap :: GetMap (MkStateMachine m) m

--------------------------------------------------------------------------------

class FromMap :: StateMap -> StateMachine -> Constraint
class FromMap m stm | m -> stm

instance fromMap :: FromMap m (MkStateMachine m)

--------------------------------------------------------------------------------

class GetState :: StateMachine -> StateName -> State -> Constraint
class GetState stm sn s

instance getState :: (GetMap stm m, Lookup m sn s) => GetState stm sn s

--------------------------------------------------------------------------------

class GetOriginState :: StateMachine -> StateName -> Row Type -> Constraint
class GetOriginState stm sn r

instance getOriginState ::
  ( GetGlobalState stm g
  , ToSym sn k
  , FilterRow g (k :> Nil') r
  ) =>
  GetOriginState stm sn r

--------------------------------------------------------------------------------

class GetAction :: StateMachine -> StateName -> ActionName -> Action -> Constraint
class GetAction stm sn an a

instance getAction ::
  ( GetState stm sn s
  , S.GetStateActions s sa
  , SA.GetAction sa an a
  ) =>
  GetAction stm sn an a

--------------------------------------------------------------------------------

class GetGlobalState :: StateMachine -> Row Type -> Constraint
class GetGlobalState stm r

instance getGlobalState :: (GetMap stm m, GetGlobalState' m r) => GetGlobalState stm r

--------------------------------------------------------------------------------

class GetGlobalState' :: Map StateName State -> Row Type -> Constraint
class GetGlobalState' m r | m -> r

instance getGlobalState'Nil :: GetGlobalState' Empty ()

instance getGlobalState'Rec ::
  ( Cons k v r' r
  , ToSym sn k
  , GetStateType s sty
  , GetType sty v
  , GetGlobalState' m r'
  ) =>
  GetGlobalState' (Insert sn s m) r

--------------------------------------------------------------------------------
-- StateMachine / Tests
--------------------------------------------------------------------------------

type KA = MkStateName "a"
type KB = MkStateName "b"

type VA = MkState (MkStateType $ Proxy "A") (MkStateActions Empty)

type VB = MkState (MkStateType $ Proxy "B") (MkStateActions Empty)

testsGetState :: Unit
testsGetState =
  fold
    [ let
        test :: forall a1 a2 a3. GetState a1 a2 a3 => Test3 a1 a2 a3
        test = test3
      in
        fold
          [ test
              (Proxy :: _ $ MkStateMachine $ Empty # Insert KA VA # Insert KA VA)
              (Proxy :: _ KA)
              (Proxy :: _ VA)
          ]
    ]

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

tests :: Unit
tests = fold [ testsGetState ]