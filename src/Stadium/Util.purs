module Stadium.Util where

import Prelude

type App :: forall a b. (a -> b) -> a -> b
type App f a
  = f a

type AppFlipped :: forall a b. a -> (a -> b) -> b
type AppFlipped a f
  = f a

infixr 0 type App as $

infixl 1 type AppFlipped as #