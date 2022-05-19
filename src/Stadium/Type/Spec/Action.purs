module Stadium.Type.Spec.Action where

import Prelude

import Type.Data.List (List')

--------------------------------------------------------------------------------
-- Action
--------------------------------------------------------------------------------

data Action

foreign import data MkAction :: Type -> List' Symbol -> Action
