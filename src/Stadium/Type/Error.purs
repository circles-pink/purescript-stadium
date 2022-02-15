module Stadium.Type.Error where

data Error

foreign import data MissingSubStateInState :: Symbol -> Error

foreign import data Protocol_InvalidTargetState :: Symbol -> Error
