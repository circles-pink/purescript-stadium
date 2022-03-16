module Stadium.Graph.ToDot
  ( graphToDot
  ) where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Dot as D
import Stadium.Graph.Type (Node(..))
import Stadium.Graph.Type as G

type Options = {
  entryPoint:: Maybe String 
}

graphToDot :: Options -> G.Graph -> D.Dot
graphToDot opt g =
  { name: g.name
  , nodes: g.nodes # reorder' # map fromNode 
  , edges: map fromEdge g.edges
  }
  where
  reorder' = case opt.entryPoint of
    Nothing -> identity
    Just ep -> reorder ep 
  
reorder :: String -> Array Node -> Array Node
reorder entryName xs = r.rest <> r.init
  where
  r = A.span (\n -> nodeToStateName n /= Just entryName) xs

  nodeToStateName :: Node -> Maybe String
  nodeToStateName n = case n of
    State {name} -> Just name 
    _ -> Nothing 
  
fromNode :: G.Node -> D.Node
fromNode n = case n of
  G.State s ->
    { id: s.name
    , shape: D.Ellipse
    , label: s.name
    }
  G.Action s ->
    { id: fromActionName s.name
    , shape: D.Box
    , label: s.name.action
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
