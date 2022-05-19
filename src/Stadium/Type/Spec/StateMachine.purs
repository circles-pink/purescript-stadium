module Stadium.Type.Spec.StateMachine where

import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Stadium.Type.Spec.State (class StateFromSpec, State)
import Stadium.Type.StateMachine (class FromMap, StateMachine, StateMap)
import Stadium.Type.StateName (class ToSym)
import Type.Map (Empty, Insert)
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)

--------------------------------------------------------------------------------
-- StateMachineSpec
--------------------------------------------------------------------------------
data StateMachineSpec

foreign import data MkStateMachineSpec :: Row State -> StateMachineSpec

--------------------------------------------------------------------------------
class GetRow :: StateMachineSpec -> Row State -> Constraint
class GetRow spec r | spec -> r

instance getRow :: GetRow (MkStateMachineSpec r) r

--------------------------------------------------------------------------------

class ToStateMachine :: StateMachineSpec -> StateMachine -> Constraint
class ToStateMachine spec stm | spec -> stm, spec -> stm where
  f :: Proxy spec -> Proxy stm

instance toStateMachine ::
  ( RowToList r rl
  , ListToRow rl r
  , GetRow spec r
  , ToStateMachineRL rl sm
  , FromMap sm stm
  ) =>
  ToStateMachine spec stm where
  f _ = Proxy :: _ stm

--------------------------------------------------------------------------------

class ToStateMachineRL :: RowList State -> StateMap -> Constraint
class ToStateMachineRL rl sm | rl -> sm, sm -> rl

instance toStateMachineRLNil :: ToStateMachineRL Nil Empty

instance toStateMachineRLRec ::
  ( ToStateMachineRL rl sm
  , ToSym sn k
  , StateFromSpec v s
  ) =>
  ToStateMachineRL (Cons k v rl) (Insert sn s sm)

