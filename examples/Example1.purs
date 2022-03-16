module Example1 where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Variant (Variant, inj)
import Language.Dot as D
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
            , action2 :: P.Action ("state2" :> Nil')
            )
      , state2 :: P.State ()
      )

type MyStateMachine
  = STM.StateMachine MyProtocol MyState MyAction

check :: Unit
check = STM.validate (Proxy :: _ MyStateMachine)

_state1 :: forall a v. a -> Variant ( state2 :: a | v )
_state1 = inj (Proxy :: _ "state2")

__state1 :: Proxy "state2"
__state1 = Proxy :: _ "state2"

myControl :: forall m. Monad m => ((MyState -> MyState) -> m Unit) -> MyState -> MyAction -> m Unit
myControl =
  C.mkControl (Proxy :: _ MyStateMachine)
    { state1:
        { action1:
            \setState _ _ -> do
              pure unit
        , action2:
            \setState _ _ -> do
              setState (\_ -> inj __state1 "sss")
              setState (\_ -> _state1 "sss")
              -- setState.state1 "foooo"
              -- setState (inj (Proxy :: _ "state1") 12)
              -- setState.state1 12
              -- setState (inj _state1 12)
              -- setState (inj (Proxy :: _ "state2") "foo")
              -- setState $ T.state1 12 
              pure unit
        }
    , state2: {}
    }

type Surface a
  = { title :: String
    , text :: String
    , prompt :: String
    , input :: String -> a
    }

--type RS a = (a -> Unit) ->
-- myView :: MyState -> Surface MyAction
-- myView =
--   mkView (Proxy :: _ MyStateMachine)
--     { state1:
--         \_ ->
--           { title: "Welcome"
--           , text: "Are you ready?"
--           , prompt: "y/n"
--           , input:
--               \i -> case i of
--                 "y" -> _yes
--                 "n" -> _no
--                 _ -> _wrongInput
--           }
-- --     }
-- type RS a
--   = (a -> Unit) -> JSX
stmProxy = Proxy :: _ MyStateMachine

main :: Effect Unit
main =
  R.reflectStateMachine (Proxy :: _ MyStateMachine)
    # G.fromStateMachineData "MyGraph"
    # G.graphToDot {entryPoint: Nothing }
    # D.printGraph
    # writeTextFile UTF8 "example-dist/example-graph.dot"
