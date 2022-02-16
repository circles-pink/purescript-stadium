module Stadium.Graph.FromStateMachine where

import Prelude
import Data.Tuple.Nested (type (/\), (/\))
import Stadium.Graph.Type as G
import Stadium.Reflect as R

type Graph'
  = { nodes :: Array G.Node
    , edges :: Array G.Edge
    }

fromStateMachineData :: String -> R.StateMachineData -> G.Graph
fromStateMachineData n sd =
  { name: n
  , nodes: findStateNodes sd
  , edges: []
  }

findStateNodes :: R.StateMachineData -> Array G.Node
findStateNodes sd = map findStateNode sd

findStateNode :: String /\ R.StateData -> G.Node
findStateNode (name /\ _) = G.State { name }
