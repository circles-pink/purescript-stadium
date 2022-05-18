module Stadium.Control where

import Prelude
import Stadium.Type

import Data.Foldable (fold)
import Data.List.Lazy (Step(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Stadium.Util (type (#))
import Type.Data.List (type (:>), Cons', List', Nil')
import Type.Map (Empty, Insert, Map)
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Type.Test (Test3, test3)

--type GetState 

--------------------------------------------------------------------------------

class GetState :: Map StateName State -> Row Type -> Constraint
class GetState stm r | stm -> r

instance getStateNil :: GetState Empty ()

instance getStateRec ::
  ( GetState stm' r'
  , Cons sn st r' r
  ) =>
  GetState (Insert (MkStateName sn) (MkState (MkStateType st) sta) stm') r

--------------------------------------------------------------------------------

class GetOriginState :: StateMachine -> StateName -> Row Type -> Constraint
class GetOriginState stm sn s | stm sn -> s

instance getOriginState :: (GetState stm st, Filter st (sn :> Nil') st') => GetOriginState (MkStateMachine stm) (MkStateName sn) st'

--------------------------------------------------------------------------------

class GetTargetState :: StateMachine -> StateName -> ActionName -> Row Type -> Constraint
class GetTargetState stm sn an s

instance getTargetState :: (GetState stm st, Filter st (sn :> Nil') st') => GetTargetState (MkStateMachine stm) (MkStateName sn) (MkActionName an) st'

--------------------------------------------------------------------------------

class Filter :: Row Type -> List' Symbol -> Row Type -> Constraint
class Filter r ss r' | ss -> r'

instance filterNil :: Filter r Nil' ()

instance filterCons ::
  ( Filter r ss r'
  , Cons s t tr r
  , Cons s t r' r''
  ) =>
  Filter r (Cons' s ss) r''

--------------------------------------------------------------------------------

class Control :: Type -> StateMachine -> StateName -> ActionName ->  -> Constraint
class Control tag stm sn an | stm -> sn an where
  control :: Proxy tag -> Proxy (Ctl stm sn an) -> 

data Ctl :: StateMachine -> StateName ->  ActionName -> Type 
data Ctl stm sn an = Ctl stm sn an

-- class Control :: Type -> StateMachine -> StateName -> ActionName -> Row Type -> Constraint
-- class
--   ( GetOriginState stm sn orSt
--   ) <=
--   Control tag stm sn an orSt
--   | stm sn orSt -> orSt
--   where
--   control
--     :: Proxy tag
--     -> Proxy stm
--     -> Proxy sn
--     -> Proxy an
--     -> (Variant orSt -> Variant orSt)
--     -> Unit
--     -> Unit
--     -> Unit

-- class Control' :: Type -> StateMachine -> StateName -> ActionName -> Constraint
-- class Control' tag stm sn an | tag -> stm sn an where
--   control' :: forall orSt taSt st at. Proxy tag -> (Variant orSt -> Variant taSt) -> Variant st -> at -> Unit

--------------------------------------------------------------------------------

tests' :: Unit
tests' =
  fold
    [ fold
        [ let
            test :: forall a1 a2. GetState a1 a2 => Proxy a1 -> Proxy a2 -> Unit
            test _ _ = unit
          in
            fold
              [ test
                  (Proxy :: _ Empty)
                  (Proxy :: _ ())
              , test
                  ( Proxy
                      :: _
                           ( Empty
                               # Insert (MkStateName "a") (MkState (MkStateType Int) (MkStateActions Empty))
                               # Insert (MkStateName "b") (MkState (MkStateType String) (MkStateActions Empty))

                           )
                  )
                  (Proxy :: _ (a :: Int, b :: String))
              ]
        , let
            test :: forall a1 a2 a3. Filter a1 a2 a3 => Test3 a1 a2 a3
            test = test3
          in
            fold
              [ test
                  (Proxy :: _ ())
                  (Proxy :: _ Nil')
                  (Proxy :: _ ())
              , test
                  (Proxy :: _ (a :: Int))
                  (Proxy :: _ ("a" :> Nil'))
                  (Proxy :: _ (a :: Int))
              , test
                  (Proxy :: _ (a :: Int, b :: String, c :: Char))
                  (Proxy :: _ ("a" :> "b" :> Nil'))
                  (Proxy :: _ (a :: Int, b :: String))
              ]
        ]
    ]

--------------------------------------------------------------------------------

-- class Control :: Type -> StateMachine -> Symbol -> Symbol -> Row Type -> Constraint
-- class
--   ( GetState stm st
--   ) <=
--   Control tag stm sn an st
--   | tag -> stm
--   where
--   control :: Proxy stm -> Proxy tag -> Variant st -> Proxy sn -> Proxy an -> Unit

-- instance c :: (GetState stm st) => Control tag stm sn an where
--   control = undefined

-- instance c :: (GetState stm st) => Control tag stm sn an where
--   control

-- class GetSet stm st ac
-- instance getSet :: GetSet stm st ac 
-- type GetSet from to = from -> to 
