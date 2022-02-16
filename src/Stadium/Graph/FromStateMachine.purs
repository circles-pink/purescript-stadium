module Stadium.Graph.FromStateMachine where

import Prelude
import Data.Array (fold, (:))
import Data.Tuple.Nested (type (/\), (/\))
import Stadium.Graph.Type as G
import Stadium.Reflect as R
import Undefined (undefined)

type Graph'
  = { nodes :: Array G.Node
    , edges :: Array G.Edge
    }

fromStateMachineData :: R.StateName -> R.StateMachineData -> G.Graph
fromStateMachineData n sd =
  { name: n
  , nodes
  , edges
  }
  where
  { nodes, edges } = fold $ map findStateNode sd

findStateNode :: R.ActionName /\ R.StateData -> Graph'
findStateNode (name /\ sd) =
  map (findActionName name) sd
    # (:)
        { nodes: [ G.State { name } ]
        , edges: []
        }
    # fold

findActionName :: R.StateName -> (R.ActionName /\ R.ActionData) -> Graph'
findActionName sn (an /\ ad) =
  map (findToState sn an) ad
    # (:)
        { nodes: [ G.Action { name: actionName } ]
        , edges: [ G.ToAction { fromState: sn, toAction: actionName } ]
        }
    # fold
  where
  actionName = { state: sn, action: an }

findToState :: R.StateName -> R.ActionName -> R.StateName -> Graph'
findToState sn an ts =
  { nodes: []
  , edges: [ G.ToState { fromAction: actionName, toState: ts } ]
  }
  where
  actionName = { state: sn, action: an }
