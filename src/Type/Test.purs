module Type.Test where

import Prelude

import Type.Proxy (Proxy)

type Test1 :: forall k. k -> Type
type Test1 a1 = Proxy a1 -> Unit

type Test2 :: forall k1 k2. k1 -> k2 -> Type
type Test2 a1 a2 = Proxy a1 -> Proxy a2 -> Unit

type Test3 :: forall k1 k2 k3. k1 -> k2 -> k3 -> Type
type Test3 a1 a2 a3 = Proxy a1 -> Proxy a2 -> Proxy a3 -> Unit

type Test4 :: forall k1 k2 k3 k4. k1 -> k2 -> k3 -> k4 -> Type
type Test4 a1 a2 a3 a4 = Proxy a1 -> Proxy a2 -> Proxy a3 -> Proxy a4 -> Unit

test1 :: forall a1. Test1 a1
test1 _ = unit

test2 :: forall a1 a2. Test2 a1 a2
test2 _ _ = unit

test3 :: forall a1 a2 a3. Test3 a1 a2 a3
test3 _ _ _ = unit

test4 :: forall a1 a2 a3 a4. Test4 a1 a2 a3 a4
test4 _ _ _ _ = unit