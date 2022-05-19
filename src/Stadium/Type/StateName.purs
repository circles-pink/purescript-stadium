module Stadium.Type.StateName where

--------------------------------------------------------------------------------
-- StateName
--------------------------------------------------------------------------------

data StateName

foreign import data MkStateName :: Symbol -> StateName

--------------------------------------------------------------------------------

class ToSym :: StateName -> Symbol -> Constraint
class ToSym sn sy | sn -> sy, sy -> sn

instance toSym :: ToSym (MkStateName sy) sy

--------------------------------------------------------------------------------