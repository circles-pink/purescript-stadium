module Stadium.Graph.FromStateMachine where

import Prelude
import Prim.Boolean (True)
import Stadium.Graph.Type as G
import Stadium.Type.Either (Right)
import Stadium.Type.StateMachine (StateMachine')
import Stadium.Type.StateMachine as STM
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

class FromStateMachine :: StateMachine' -> Constraint
class FromStateMachine stm where
  fromStateMachine :: Proxy stm -> G.Graph

instance fromStateMachine' ::
  ( TypeEquals stm (STM.StateMachine ptc st ac)
  , STM.Validate stm (Right True)
  ) =>
  FromStateMachine stm where
  fromStateMachine _ = undefined
