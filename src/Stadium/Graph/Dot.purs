module Stadium.Graph.Dot
  ( graphToDot
  ) where

import Prelude
import Dot as D
import Stadium.Graph as G

graphToDot :: G.Graph -> D.Dot
graphToDot g =
  { name: g.name
  , nodes: map fromNode g.nodes
  , edges: map fromEdge g.edges
  }

fromNode :: G.Node -> D.Node
fromNode n = case n of
  G.State s ->
    { id: s.name
    , shape: D.Ellipse
    }
  G.Action s ->
    { id: fromActionName s.name
    , shape: D.Ellipse
    }

fromEdge :: G.Edge -> D.Edge
fromEdge n = case n of
  G.ToState s ->
    { from: fromActionName s.fromAction
    , to: s.toState
    }
  G.ToAction s ->
    { from: s.fromState
    , to: fromActionName s.toAction
    }

fromActionName :: G.ActionName -> String
fromActionName a = a.state <> "__" <> a.action
