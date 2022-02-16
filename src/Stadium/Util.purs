module Stadium.Util where

import Prelude

type App :: forall a b. (a -> b) -> a -> b
type App f a
  = f a

infixr 0 type App as $

type Compose :: forall a b c. (b -> c) -> (a -> b) -> a -> c
type Compose f g a
  = f (g a)

infixr 9 type Compose as <<<
