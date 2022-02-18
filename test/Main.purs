module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Stadium.Control as Stadium.Control
import Test.Unit.Main (runTest)

main :: Effect Unit
main =
  runTest do
    Stadium.Control.tests
