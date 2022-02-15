module Stadium.Type.Protocol.GetKeys where

import Stadium.Class.KeysOf (class KeysOf)
import Stadium.Type.Protocol.Type (Protocol, Protocol')
import Type.Data.List (List')

--------------------------------------------------------------------------------
-- class GetKeys
--------------------------------------------------------------------------------
instance getKeys :: (KeysOf r ks) => GetKeys (Protocol r) ks

class GetKeys :: Protocol' -> List' Symbol -> Constraint
class GetKeys ptc ks | ptc -> ks
