module Type.Map where

import Prelude

import Data.Foldable (fold)
import Stadium.Util (type (#))
import Type.Proxy (Proxy(..))
import Type.Test (Test3, test3)

data Map :: forall k1 k2. k1 -> k2 -> Type
data Map k v

foreign import data Empty :: forall k v. Map k v

foreign import data Insert :: forall k v. k -> v -> Map k v -> Map k v

--

class Lookup :: forall k v. Map k v -> k -> v -> Constraint
class Lookup map k v

instance lookupRec :: Lookup (Insert k v m) k v
else instance lookupRec' :: Lookup m k v => Lookup (Insert k' v' m) k v

--------------------------------------------------------------------------------

tests' :: Unit
tests' =
  let
    test :: forall a1 a2 a3. Lookup a1 a2 a3 => Test3 a1 a2 a3
    test = test3
  in
    fold
      [ test
          (Proxy :: _ (Empty # Insert "a" Int # Insert "b" String))
          (Proxy :: _ "a")
          (Proxy :: _ Int)
      ]