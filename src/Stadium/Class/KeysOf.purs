module Stadium.Class.KeysOf
  ( class KeysOf
  , class KeysOf'
  , tests
  ) where

import Prelude
import Type.Data.List (type (:>), Cons', List', Nil')
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Type.Proxy (Proxy(..))

class KeysOf :: forall k. Row k -> List' Symbol -> Constraint
class KeysOf r syms | r -> syms

instance keysOf :: (RowToList r rl, KeysOf' rl syms) => KeysOf r syms

class KeysOf' :: forall k. RowList k -> List' Symbol -> Constraint
class KeysOf' rl syms | rl -> syms

instance keysOf'Nil :: KeysOf' Nil Nil'

instance keysOf'Cons ::
  ( KeysOf' tail syms
    ) =>
  KeysOf' (Cons s t tail) (Cons' s syms)

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
test :: forall r syms. KeysOf r syms => Proxy r -> Proxy syms -> Unit
test _ _ = unit

tests :: Unit
tests =
  unit
    <> test
        (Proxy :: _ ())
        (Proxy :: _ Nil')
    <> test
        (Proxy :: _ ( a :: Int, b :: String ))
        (Proxy :: _ ("a" :> "b" :> Nil'))
    <> test
        (Proxy :: _ ( b :: Int, a :: String ))
        (Proxy :: _ ("a" :> "b" :> Nil'))
