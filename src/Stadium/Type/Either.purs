module Stadium.Type.Either
  ( A(..)
  , A1(..)
  , A2(..)
  , B(..)
  , B1(..)
  , B2(..)
  , Either
  , F(..)
  , Left
  , Right
  , class First
  , class First3
  , class Match
  , class LMap
  , testsFirst
  , testsMatch
  ) where

import Prelude
import Type.Proxy (Proxy(..))

data Either :: forall a b. a -> b -> Type
data Either a b

foreign import data Left :: forall a b. a -> Either a b

foreign import data Right :: forall a b. b -> Either a b

--------------------------------------------------------------------------------
-- class First
--------------------------------------------------------------------------------
instance firstRightRight :: First (Right b1) (Right b2) (Right b1)

instance firstRightLeft :: First (Right b1) (Left a) (Left a)

instance firstLeftRight :: First (Left a) (Right b) (Left a)

instance firstLeftLeft :: First (Left a1) (Left a2) (Left a1)

class First :: forall a b. Either a b -> Either a b -> Either a b -> Constraint
class First e1 e2 o | e1 e2 -> o

--------------------------------------------------------------------------------
-- class First3
--------------------------------------------------------------------------------
instance first3 ::
  ( First e1 e2 o1
  , First e2 e3 o2
  , First o1 o2 o
  ) =>
  First3 e1 e2 e3 o

class First3 :: forall a b. Either a b -> Either a b -> Either a b -> Either a b -> Constraint
class First3 e1 e2 e3 o | e1 e2 e3 -> o

--------------------------------------------------------------------------------
-- class Match
--------------------------------------------------------------------------------
instance matchLeft :: Match (Left a) onLeft onRight (onLeft a)

instance matchRight :: Match (Right b) onLeft onRight (onRight b)

class Match :: forall a b z. Either a b -> (a -> z) -> (b -> z) -> z -> Constraint
class Match e onLeft onRight o | e onLeft onRight -> o

--------------------------------------------------------------------------------
-- class Lmap
--------------------------------------------------------------------------------
instance lmapLeft :: LMap (Left a) mapLeft (Left (mapLeft a))

instance lmapRight :: LMap (Right b) mapLeft (Right b)

class LMap :: forall a a' b. Either a b -> (a -> a') -> Either a' b -> Constraint
class LMap e mapLeft o | e mapLeft -> o

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
data A

data B

data B1

data B2

data A1

data A2

data F a

data G a

testsFirst :: Unit
testsFirst =
  unit
    <> testFirst
        (Proxy :: _ (Right B1))
        (Proxy :: _ (Right B2))
        (Proxy :: _ (Right B1))
    <> testFirst
        (Proxy :: _ (Right B))
        (Proxy :: _ (Left A))
        (Proxy :: _ (Left A))
    <> testFirst
        (Proxy :: _ (Left A))
        (Proxy :: _ (Right B))
        (Proxy :: _ (Left A))
    <> testFirst
        (Proxy :: _ (Left A1))
        (Proxy :: _ (Left A2))
        (Proxy :: _ (Left A1))
  where
  testFirst :: forall a b c. First a b c => Proxy a -> Proxy b -> Proxy c -> Unit
  testFirst _ _ _ = unit

testsMatch :: Unit
testsMatch =
  unit
    <> testMatch
        (Proxy :: _ (Left A))
        (Proxy :: _ F)
        (Proxy :: _ G)
        (Proxy :: _ (F A))
    <> testMatch
        (Proxy :: _ (Right A))
        (Proxy :: _ F)
        (Proxy :: _ G)
        (Proxy :: _ (G A))
  where
  testMatch :: forall a b c d. Match a b c d => Proxy a -> Proxy b -> Proxy c -> Proxy d -> Unit
  testMatch _ _ _ _ = unit

testsLMap :: Unit
testsLMap =
  unit
    <> testLMap
        (Proxy :: _ (Left A))
        (Proxy :: _ F)
        (Proxy :: _ (Left (F A)))
    <> testLMap
        (Proxy :: _ (Right A))
        (Proxy :: _ F)
        (Proxy :: _ (Right A))
  where
  testLMap :: forall a b c. LMap a b c => Proxy a -> Proxy b -> Proxy c -> Unit
  testLMap _ _ _ = unit
