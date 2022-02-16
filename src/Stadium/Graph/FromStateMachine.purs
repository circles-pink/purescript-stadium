module Stadium.Graph.FromStateMachine where

import Prelude
import Prim.Boolean (True)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Stadium.Graph.Type as G
import Stadium.Type.Either (Right)
import Stadium.Type.Protocol as P
import Stadium.Type.StateMachine (StateMachine')
import Stadium.Type.StateMachine as STM
import Stadium.Type.Tuple (type (/\))
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

type Graph'
  = { nodes :: Array G.Node
    , edges :: Array G.Edge
    }

--------------------------------------------------------------------------------
-- class FromStateMachine
--------------------------------------------------------------------------------
instance fromStateMachine' ::
  ( TypeEquals stm (STM.StateMachine (P.Protocol states) st ac)
  , STM.Validate stm (Right True)
  , RowToList states states'
  , MapStates states'
  ) =>
  FromStateMachine stm where
  fromStateMachine name _ = { name, nodes, edges }
    where
    { nodes, edges } = mapStates (Proxy :: _ states')

class FromStateMachine :: StateMachine' -> Constraint
class FromStateMachine stm where
  fromStateMachine :: String -> Proxy stm -> G.Graph

--------------------------------------------------------------------------------
-- class MapStates
--------------------------------------------------------------------------------
instance mapStatesNil :: MapStates Nil where
  mapStates _ = { nodes: [], edges: [] }

instance mapStatesCons ::
  ( MapStates tail
  , MapState s t
  ) =>
  MapStates (Cons s t tail) where
  mapStates _ =
    mapState (Proxy :: _ (s /\ t))
      <> mapStates (Proxy :: _ tail)

class MapStates :: RowList P.State' -> Constraint
class MapStates states where
  mapStates :: Proxy states -> Graph'

--------------------------------------------------------------------------------
-- class MapState
--------------------------------------------------------------------------------
instance mapState' ::
  ( RowToList st st'
  , MapActions s st'
  ) =>
  MapState s (P.State st) where
  mapState _ = mapActions (Proxy :: _ s) (Proxy :: _ st')

class MapState :: Symbol -> P.State' -> Constraint
class MapState n st where
  mapState :: Proxy (n /\ st) -> Graph'

--------------------------------------------------------------------------------
-- class MapActions
--------------------------------------------------------------------------------
instance mapActionsNil :: MapActions sn Nil where
  mapActions _ _ = { nodes: [], edges: [] }

instance mapActionsCons :: (MapActions sn tail) => MapActions sn (Cons s t tail) where
  mapActions _ _ =
    mapAction (Proxy :: _ sn) (Proxy :: _ (s /\ t))
      <> mapActions (Proxy :: _ sn) (Proxy :: _ tail)

class MapActions :: P.StateName -> RowList P.Action' -> Constraint
class MapActions sn actions where
  mapActions :: Proxy sn -> Proxy actions -> Graph'

--------------------------------------------------------------------------------
-- class MapAction
--------------------------------------------------------------------------------
instance mapAction' :: MapAction sn an ac where
  mapAction _ _ = undefined

class MapAction :: P.StateName -> P.ActionName -> P.Action' -> Constraint
class MapAction sn an ac where
  mapAction :: Proxy sn -> Proxy (an /\ ac) -> Graph'
