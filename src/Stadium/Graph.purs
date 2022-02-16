module Stadium.Graph where

import Prelude

type StateName
  = String

type ActionName
  = String

data Node
  = Action { state :: StateName, action :: ActionName }
  | State { state :: StateName }

data Edge
  = ToState { fromAction :: ActionName, toState :: StateName }
  | ToAction { fromState :: StateName, toAction :: ActionName }

type Graph
  = { nodes :: Array Node
    , edges :: Array Edge
    }
