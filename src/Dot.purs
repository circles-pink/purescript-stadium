module Dot where

import Prelude
import Data.Array (fold)

data Shape
  = Box
  | Ellipse

type Node
  = { id :: String
    , label :: String
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
fromNode n =
  "  " <> show n.id
    <> " [shape="
    <> fromShape n.shape
    <> ", "
    <> "label="
    <> show n.label
    <> ", "
    <> "width="
    <> show (shapeWidth n.shape)
    <> ", "
    <> "height="
    <> show (shapeHeight n.shape)
    <> "];\n"

fromShape :: Shape -> String
fromShape s = case s of
  Box -> "box"
  Ellipse -> "ellipse"

shapeWidth :: Shape -> Number
shapeWidth s = case s of
  Box -> 0.5
  Ellipse -> 1.0

shapeHeight :: Shape -> Number
shapeHeight s = case s of
  Box -> 0.25
  Ellipse -> 0.5
