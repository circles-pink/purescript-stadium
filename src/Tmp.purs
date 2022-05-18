module Tmp where

import Prelude

import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

class TypeEquals a b <= MyC a b | a -> b where
  control :: Proxy a -> b

class MyC' a where
  control 

instance x :: MyC Int a where
  control _ = 2