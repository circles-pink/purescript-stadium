module Stadium.Graph.ToDot
  ( graphToDot
  ) where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..))
import Language.Dot as D
import Stadium.Graph.Type (Node(..))
import Stadium.Graph.Type as G

type Options
  = { entryPoint :: Maybe String
    }

graphToDot :: Options -> G.Graph -> D.Graph
graphToDot opt g =
  D.Graph
    { strict: true
    , type: D.directed_
    , id: Just $ D.Id g.name
    , stmts:
        (g.nodes # reorder' # map (fromNode >>> D.nodeStmt))
          <> (map (fromEdge >>> D.edgeStmt) g.edges)
    }
  where
  reorder' = case opt.entryPoint of
    Nothing -> identity
    Just ep -> reorder ep

fromNode :: G.Node -> D.NodeStmt
fromNode n = case n of
  G.State s ->
    D.NodeStmt
      { id: D.NodeId { id: D.Id s.name }
      , attrs:
          [ D.label s.name
          , D.unsafeAttr "shape" "rectangle"
          , D.unsafeAttr "style" "filled"
          , D.unsafeAttr "fontcolor" "white"
          , D.unsafeAttr "fillcolor" "#7776B8"
          ]
      }
  G.Action s ->
    D.NodeStmt
      { id: D.NodeId { id: D.Id $ fromActionName s.name }
      , attrs:
          [ D.label s.name.action
          , D.unsafeAttr "shape" "rectangle"
          , D.unsafeAttr "height" "0.05"
          , D.unsafeAttr "width" "0.05"
          , D.unsafeAttr "fontsize" "5"
          ]
      }

fromEdge :: G.Edge -> D.EdgeStmt
fromEdge n = case n of
  G.ToState s ->
    D.EdgeStmt
      (D.nodeId $ D.NodeId { id: D.Id $ fromActionName s.fromAction })
      (D.nodeId $ D.NodeId { id: D.Id s.toState })
      []
      [ D.unsafeAttr "arrowsize" "0.5" ]
  G.ToAction s ->
    D.EdgeStmt
      (D.nodeId $ D.NodeId { id: D.Id s.fromState })
      (D.nodeId $ D.NodeId { id: D.Id $ fromActionName s.toAction })
      []
      [ D.unsafeAttr "arrowsize" "0.5" ]

fromActionName :: G.ActionName -> String
fromActionName a = a.state <> "__" <> a.action

reorder :: String -> Array Node -> Array Node
reorder entryName xs = r.rest <> r.init
  where
  r = A.span (\n -> nodeToStateName n /= Just entryName) xs

  nodeToStateName :: Node -> Maybe String
  nodeToStateName n = case n of
    State { name } -> Just name
    _ -> Nothing
