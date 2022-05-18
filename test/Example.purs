module Test.Example where

import Prelude
import Stadium.Type

import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant, inj, match, onMatch)
import Stadium.Control (class Control, class GetControl, class GetOriginState, class GetTargetState)
import Stadium.Util (type (#))
import Type.Equality (class TypeEquals)
import Type.Map (Empty, Insert)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

-- import Undefined (undefined)

type MyStateMachine =
  MkStateMachine
    ( Empty
        # Insert (MkStateName "on")
            ( MkState
                (MkStateType Unit)
                ( MkStateActions
                    ( Empty
                        # Insert (MkActionName "turnOff")
                            ( MkAction
                                (MkActionType Unit)
                                ( MkTargets
                                    # AddTarget (MkTarget (MkStateName "off"))

                                )
                            )
                    )

                )
            )
        # Insert (MkStateName "off")
            ( MkState
                (MkStateType Unit)
                ( MkStateActions
                    ( Empty
                        # Insert (MkActionName "turnOn")
                            ( MkAction
                                (MkActionType Unit)
                                ( MkTargets
                                    # AddTarget (MkTarget (MkStateName "on"))

                                )
                            )
                    )

                )
            )
    )

-- type MyStateMachine =
--   MkStateMachine
--     ( "on" ::
--         MkState
--           (MkStateName "on")
--           (MkStateType Unit)
--           ( MkStateActions
--               ( "turnOff" ::
--                   MkAction
--                     (MkActionName "turnOff")
--                     (MkActionType Unit)
--                     ( MkTargets
--                         # AddTarget (MkTarget (MkStateName "off"))
--                     )

--               )

--           )
--     )

newtype Tag = Tag (Proxy "stm")

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

turnOn :: forall r. Unit -> Variant (turnOn :: Unit | r)
turnOn = inj (Proxy :: _ "turnOn")

turnOff :: forall r. Unit -> Variant (turnOff :: Unit | r)
turnOff = inj (Proxy :: _ "turnOff")

_stm = (Proxy :: _ MyStateMachine)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

instance my :: GetControl stm sn an f => Control Tag stm sn an where
  control _ _ = undefined :: f

--------------------------------------------------------------------------------

-- mk
--   :: forall stm sn an orSt taSt
--    . GetOriginState stm sn orSt
--   => GetTargetState stm sn an taSt
--   => ((Variant orSt -> Variant taSt) -> Unit -> Unit -> Unit)
--   -> Proxy stm
--   -> Proxy sn
--   -> Proxy an
--   -> ((Variant orSt -> Variant taSt) -> Unit -> Unit -> Unit)
-- mk = undefined

-- instance controlOnTurnOff ::
--   GetOriginState MyStateMachine (MkStateName "on") orSt =>
--   Control
--     Tag
--     MyStateMachine
--     (MkStateName "on")
--     (MkActionName "turnOff")
--     orSt
--   where
--   control _ _ _ _ set st ac =
--     let
--       x = set (inj (Proxy :: _ "foo") unit)
--     in
--       unit

-- else instance controlOffTurnOn ::
--   Control
--     Tag
--     MyStateMachine
--     (MkStateName "off")
--     (MkActionName "turnOn")
--     orSt
--     taSt
--   where
--   control = undefined

--------------------------------------------------------------------------------

-- data Res stm a = Res (Proxy stm) a

-- unRes :: forall stm a. Res stm a -> a
-- unRes (Res _ a) = a

-- class ControlFn stm f | stm -> f

-- instance c :: GetState stm st => ControlFn stm ((Variant st -> Variant st) -> Variant st -> Unit)

-- mk :: forall stm f f2. Int -> T stm
-- mk x = undefined

-- type T stm = forall f. ControlFn stm f => Res stm f

-- on_turnOff :: T MyStateMachine
-- on_turnOff = mk f
--   where
--   f = 1

-- x = (unRes on_turnOff) (\s -> s) (inj (Proxy :: _ "off") unit)

-- instance on_turnOff :: Control Tag MyStateMachine "on" "turnOff" st where
--   control :: _ -> _ -> Variant st -> _ -> _ -> _
--   control _ _ st _ _ = ?a st

-- instance off_turnOn :: Control Tag MyStateMachine "off" "turnOn" where
--   control _ _ _ _ _ = unit

--   control set _ _ = set (\_ -> turnOn unit) 

---
