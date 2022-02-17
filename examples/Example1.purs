module Example1
  ( MyAction
  , MyProtocol
  , MyState
  , check
  , main
  ) where

import Prelude
import Data.Variant (Variant)
import Dot as D
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Stadium.Control as C
import Stadium.Graph as G
import Stadium.Reflect as R
import Stadium.Type.Protocol as P
import Stadium.Type.StateMachine as STM
import Type.Data.List (type (:>), Nil')
import Type.Proxy (Proxy(..))

type MyState
  = Variant
      ( state1 :: Int
      , state2 :: String
      )

type MyAction
  = Variant
      ( state1 ::
          Variant
            ( action1 :: Int
            , action2 :: Boolean
            )
      , state2 :: Variant ()
      )

type MyProtocol
  = P.Protocol
      ( state1 ::
          P.State
            ( action1 :: P.Action Nil'
            , action2 :: P.Action ("state2" :> "state1" :> Nil')
            )
      , state2 :: P.State ()
      )

type MyStateMachine
  = STM.StateMachine MyProtocol MyState MyAction

check :: Unit
check = STM.validate (Proxy :: _ MyStateMachine)

myControl :: forall m. C.Control MyState MyAction m
myControl =
  C.mkControl (Proxy :: _ MyStateMachine)
    {}

main :: Effect Unit
main =
  R.reflectStateMachine (Proxy :: _ MyStateMachine)
    # G.fromStateMachineData "MyGraph"
    # G.graphToDot
    # D.toString
    # writeTextFile UTF8 "example-dist/example-graph.dot"
