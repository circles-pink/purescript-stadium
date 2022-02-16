module Stadium.Graph.FromStateMachine where

import Prelude
import Prim.Boolean (True)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Stadium.Graph.Type as G
import Stadium.Reflect as R
import Stadium.Type.Either (Right)
import Stadium.Type.Protocol as P
import Stadium.Type.StateMachine (StateMachine')
import Stadium.Type.StateMachine as STM
import Stadium.Type.Tuple (type (/\))
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

fromStateMachineData :: String -> R.StateMachineData -> G.Graph
fromStateMachineData _ = undefined
