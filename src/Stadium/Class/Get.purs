module Stadium.Class.Get where

import Prelude
import Data.Array (fold)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Stadium.Type.Either (Either, Left, Right)
import Stadium.Type.Unit (MkUnit, Unit')
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- class Get'
--------------------------------------------------------------------------------
instance get'Nil :: Get' Nil s (Left MkUnit)

instance get'ConsOk :: Get' (Cons s t tail) s (Right t)
else instance get'ConsErr :: (Get' tail s o) => Get' (Cons s' t tail) s o

class Get' :: forall a. RowList a -> Symbol -> Either Unit' a -> Constraint
class Get' rl s o | rl s -> o

--------------------------------------------------------------------------------
-- class Get
--------------------------------------------------------------------------------
instance get :: (RowToList r rl, Get' rl s o) => Get r s o

class Get :: forall a. Row a -> Symbol -> Either Unit' a -> Constraint
class Get r s o | r s -> o

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
data A

data B

data C

tests :: Unit
tests =
  let
    test :: forall a1 a2 a3. Get a1 a2 a3 => Proxy a1 -> Proxy a2 -> Proxy a3 -> Unit
    test _ _ _ = unit
  in
    fold
      [ test
          (Proxy :: _ ( a :: A, b :: B, c :: C ))
          (Proxy :: _ "b")
          (Proxy :: _ (Right B))
      , test
          (Proxy :: _ ( a :: A, b :: B, c :: C ))
          (Proxy :: _ "d")
          (Proxy :: _ (Left MkUnit))
      ]
