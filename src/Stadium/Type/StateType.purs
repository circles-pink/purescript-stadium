module Stadium.Type.StateType where

--------------------------------------------------------------------------------
-- StateType
--------------------------------------------------------------------------------

data StateType

foreign import data MkStateType :: Type -> StateType

--------------------------------------------------------------------------------

class GetType :: StateType -> Type -> Constraint
class GetType stt t

instance getType :: GetType (MkStateType t) t

--------------------------------------------------------------------------------