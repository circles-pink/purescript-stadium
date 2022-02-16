module Stadium.Graph.Type where

type StateName
  = String

type ActionName
  = { state :: StateName
    , action :: String
    }

data Node
  = Action { name :: ActionName }
  | State { name :: StateName }

data Edge
  = ToState { fromAction :: ActionName, toState :: StateName }
  | ToAction { fromState :: StateName, toAction :: ActionName }

type Graph
  = { name :: String
    , nodes :: Array Node
    , edges :: Array Edge
    }


