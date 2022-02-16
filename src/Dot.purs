module Dot where

import Prelude
import Data.Array (fold)

data Shape
  = Box
  | Ellipse

type Node
  = { id :: String
    , shape :: Shape
    }

type Edge
  = { from :: String
    , to :: String
    }

type Dot
  = { name :: String
    , nodes :: Array Node
    , edges :: Array Edge
    }

toString :: Dot -> String
toString d = head <> nodes <> edges <> tail
  where
  head = "digraph " <> show d.name <> " {\n"

  nodes = fold $ map fromNode d.nodes

  edges = fold $ map fromEdge d.edges

  tail = "}\n"

fromEdge :: Edge -> String
fromEdge e = "  " <> show e.from <> " -> " <> show e.to <> ";\n"

fromNode :: Node -> String
fromNode n = "  " <> show n.id <> " [shape=" <> fromShape n.shape <> "];\n"

fromShape :: Shape -> String
fromShape s = case s of
  Box -> "box"
  Ellipse -> "ellipse"
