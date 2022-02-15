module Stadium.Type.Tuple where

import Prelude

data Tuple :: forall a b. a -> b -> Type
data Tuple a b

infixr 6 type Tuple as /\
