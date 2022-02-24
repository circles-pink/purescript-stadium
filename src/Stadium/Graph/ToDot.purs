module Stadium.Graph.ToDot
  ( graphToDot
  ) where

import Prelude
import Data.DotLang ((=*>), (==>))
import Data.DotLang as D
import Data.DotLang.Attr.Edge as DE
import Data.DotLang.Attr.Node as DN
import Stadium.Graph.Type as G
import Undefined (undefined)

graphToDot :: G.Graph -> D.Graph
graphToDot g =
  D.DiGraph
    (map fromNode g.nodes <> map fromEdge g.edges)

fromNode :: G.Node -> D.Definition
fromNode = case _ of
  G.State st ->
    D.node st.name
      [ DN.Shape DN.Ellipse
      , DN.Label $ DN.TextLabel st.name
      ]
  G.Action ac ->
    D.node (fromActionName ac.name)
      [ DN.Shape DN.Box
      , DN.Label $ DN.TextLabel ac.name.action
      ]

fromEdge :: G.Edge -> D.Definition
fromEdge = case _ of
  G.ToState st ->
    (fromActionName st.fromAction) =*> st.toState
      $ []
  G.ToAction ac ->
    ac.fromState =*> (fromActionName ac.toAction)
      $ []

fromActionName :: G.ActionName -> String
fromActionName a = a.state <> "__" <> a.action
