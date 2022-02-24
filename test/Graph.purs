module Test.Graph where

import Prelude
import Data.Variant (Variant)
import Dot as D
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Stadium.Graph as G
import Stadium.Reflect as R
import Stadium.Type.Protocol as P
import Stadium.Type.StateMachine as STM
import Test.Unit as T
import Test.Unit.Assert as A
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
            , action2 :: P.Action ("state2" :> Nil')
            )
      , state2 :: P.State ()
      )

type MyStateMachine
  = STM.StateMachine MyProtocol MyState MyAction

tests :: T.TestSuite
tests =
  T.suite "Graph" do
    T.suite "golden tests" do
      T.test "graph1" do
        expected <- readTextFile UTF8 "test/golden/graph1.dot"
        let
          actual =
            R.reflectStateMachine (Proxy :: _ MyStateMachine)
              # G.fromStateMachineData "MyGraph"
              # G.graphToDot
              # D.toString
        writeTextFile UTF8 "test/golden/graph1-actual.dot" actual
        A.equal expected actual
