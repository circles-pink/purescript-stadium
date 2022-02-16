module Stadium.Type.Protocol.Type where

import Prelude
import Prim.Boolean (True)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.TypeError (class Fail, class Warn, Beside, Doc, Text)
import Stadium.Class.KeysOf (class KeysOf)
import Stadium.Type.Either (class First, Either, Right, Left)
import Type.Data.Boolean (class If)
import Type.Data.List (class IsMember, type (:>), Cons', List', Nil')
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
foreign import data Protocol :: Row State' -> Protocol'

foreign import data State :: Row Action' -> State'

foreign import data Action :: List' Symbol -> Action'

data Protocol' :: Type
data Protocol'

data State' :: Type
data State'

data Action' :: Type
data Action'

type StateName
  = Symbol

type ActionName
  = Symbol

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
infixl 1 type Snoc' as <:

type Snoc' :: forall k. List' k -> k -> List' k
type Snoc' xs x
  = Cons' x xs

--------------------------------------------------------------------------------
-- Sample
--------------------------------------------------------------------------------
type T
  = Protocol
      ( state1 ::
          State
            ( action1 ::
                Action
                  ( Nil'
                      <: "s"
                      <: "a"
                  )
            , action2 ::
                Action
                  ( Nil'
                      <: "s"
                      <: "a"
                  )
            , action3 ::
                Action
                  ( Nil'
                      <: "s"
                      <: "a"
                  )
            )
      , state2 :: State ()
      )
