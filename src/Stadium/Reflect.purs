module Stadium.Reflect where

import Prelude
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Prim.Boolean (True)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Stadium.Type.Either (Right)
import Stadium.Type.Protocol as P
import Stadium.Type.StateMachine (StateMachine')
import Stadium.Type.StateMachine as STM
import Type.Data.List (Cons', List', Nil')
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

type StateName
  = String

type ActionName
  = String

type ActionData
  = Array StateName

type StateData
  = Array (ActionName /\ ActionData)

type StateMachineData
  = Array (StateName /\ StateData)

--------------------------------------------------------------------------------
-- class Reflect
--------------------------------------------------------------------------------
instance reflect' ::
  ( TypeEquals stm (STM.StateMachine (P.Protocol states) st ac)
  , STM.Validate stm (Right True)
  , RowToList states states'
  , MapStates states'
  ) =>
  Reflect stm where
  reflectStateMachine _ = mapStates (Proxy :: _ states')

class Reflect :: StateMachine' -> Constraint
class Reflect stm where
  reflectStateMachine :: Proxy stm -> StateMachineData

--------------------------------------------------------------------------------
-- class MapStates
--------------------------------------------------------------------------------
instance mapStatesNil :: MapStates Nil where
  mapStates _ = []

instance mapStatesCons ::
  ( MapStates tail
  , MapState s t
  ) =>
  MapStates (Cons s t tail) where
  mapStates _ =
    [ mapState (Proxy :: _ s) (Proxy :: _ t) ]
      <> mapStates (Proxy :: _ tail)

class MapStates :: RowList P.State' -> Constraint
class MapStates states where
  mapStates :: Proxy states -> StateMachineData

--------------------------------------------------------------------------------
-- class MapState
--------------------------------------------------------------------------------
instance mapState' ::
  ( RowToList st st'
  , MapActions st'
  , IsSymbol s
  ) =>
  MapState s (P.State st) where
  mapState _ _ = reflectSymbol (Proxy :: _ s) /\ mapActions (Proxy :: _ st')

class MapState :: Symbol -> P.State' -> Constraint
class MapState n st where
  mapState :: Proxy n -> Proxy st -> (String /\ StateData)

--------------------------------------------------------------------------------
-- class MapActions
--------------------------------------------------------------------------------
instance mapActionsNil :: MapActions Nil where
  mapActions _ = []

instance mapActionsCons ::
  ( MapActions tail
  , MapAction s t
  , IsSymbol s
  ) =>
  MapActions (Cons s t tail) where
  mapActions _ =
    [ mapAction (Proxy :: _ s) (Proxy :: _ t) ]
      <> mapActions (Proxy :: _ tail)

class MapActions :: RowList P.Action' -> Constraint
class MapActions actions where
  mapActions :: Proxy actions -> StateData

--------------------------------------------------------------------------------
-- class MapAction
--------------------------------------------------------------------------------
instance mapAction' ::
  ( IsSymbol an
  , MapTargetStates ac
  ) =>
  MapAction an (P.Action ac) where
  mapAction _ _ = reflectSymbol (Proxy :: _ an) /\ mapTargetStates (Proxy :: _ ac)

class MapAction :: P.ActionName -> P.Action' -> Constraint
class MapAction an ac where
  mapAction :: Proxy an -> Proxy ac -> (ActionName /\ ActionData)

--------------------------------------------------------------------------------
-- class MapTargetStates
--------------------------------------------------------------------------------
instance mapTargetStatesNil :: MapTargetStates Nil' where
  mapTargetStates _ = []

instance mapTargetStatesCons ::
  ( MapTargetStates ts
  , IsSymbol t
  ) =>
  MapTargetStates (Cons' t ts) where
  mapTargetStates _ =
    [ reflectSymbol (Proxy :: _ t) ]
      <> mapTargetStates (Proxy :: _ ts)

class MapTargetStates :: List' Symbol -> Constraint
class MapTargetStates xs where
  mapTargetStates :: Proxy xs -> ActionData
