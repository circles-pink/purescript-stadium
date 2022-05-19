module Test.Example where

import Prelude

import Data.Variant (Variant, inj)
import Stadium.Type.Spec as SP
import Type.Data.List (type (:>), Nil')
import Type.Proxy (Proxy(..))

-- import Undefined (undefined)

type Spec = SP.MkStateMachineSpec
  ( "on" ::
      SP.MkState Unit
        ( "turnOff" :: SP.MkAction Unit ("off" :> Nil')
        , "toggle" :: SP.MkAction Unit ("on" :> "off" :> Nil')
        )
  , "off" ::
      SP.MkState Unit
        ("turnOn" :: SP.MkAction Unit ("on" :> Nil'))
  )

-- type MyStateMachine =
--   MkStateMachine
--     ( Empty
--         # Insert (MkStateName "on")
--             ( MkState
--                 (MkStateType Unit)
--                 ( MkStateActions
--                     ( Empty
--                         # Insert (MkActionName "turnOff")
--                             ( MkAction
--                                 (MkActionType Unit)
--                                 ( MkTargets
--                                     (Nil' # Cons' (MkStateName "off"))
--                                 )
--                             )
--                     )

--                 )
--             )
--         # Insert (MkStateName "off")
--             ( MkState
--                 (MkStateType Unit)
--                 ( MkStateActions
--                     ( Empty
--                         # Insert (MkActionName "turnOn")
--                             ( MkAction
--                                 (MkActionType Unit)
--                                 ( MkTargets
--                                     (Nil' # Cons' (MkStateName "on"))
--                                 )
--                             )
--                     )

--                 )
--             )
--     )

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

on :: forall r. Unit -> Variant (on :: Unit | r)
on = inj (Proxy :: _ "on")

off :: forall r. Unit -> Variant (off :: Unit | r)
off = inj (Proxy :: _ "off")

--_stm = (Proxy :: _ MyStateMachine)

--------------------------------------------------------------------------------

-- data Res :: StateMachine -> StateName -> ActionName -> (Type -> Type) -> Type -> Type
-- data Res stm sn an m a = Res (Proxy stm) (Proxy sn) (Proxy an) (Proxy m) a

-- type Res' :: StateMachine -> StateName -> ActionName -> Type -> Type
-- type Res' stm sn an a = a

-- type TControlHandler orSt taSt st ac m = ((Variant orSt -> Variant taSt) -> m Unit) -> st -> ac -> m Unit

-- class ControlHandler :: StateMachine -> StateName -> ActionName -> (Type -> Type) -> Type -> Constraint
-- class ControlHandler stm sn an m f | stm sn an -> f

-- type Ac = Unit

-- instance controlHandler ::
--   ( GetState stm st
--   , GetOriginState stm sn orSt
--   , GetTargetState stm sn an taSt
--   , Monad m
--   ) =>
--   ControlHandler stm sn an m (((Variant orSt -> Variant taSt) -> m Unit) -> Variant st -> Ac -> m Unit)

-- mkControl
--   :: forall stm sn an st orSt taSt m
--    . GetState stm st
--   => GetOriginState stm sn orSt
--   => GetTargetState stm sn an taSt
--   => Monad m
--   => (((Variant orSt -> Variant taSt) -> m Unit) -> Variant st -> Ac -> m Unit)
--   -> T stm sn an m
-- mkControl f = undefined

-- --------------------------------------------------------------------------------

-- type T :: StateMachine -> StateName -> ActionName -> (Type -> Type) -> Type
-- type T stm sn an m = forall f. ControlHandler stm sn an m f => Res stm sn an m f

-- control_on_turnOff :: forall m. Monad m => T MyStateMachine (MkStateName "on") (MkActionName "turnOff") m
-- control_on_turnOff = mkControl f
--   where
--   f set _ st = do
--     set (\s -> off unit)
--     pure unit

-- control_off_turnOn :: forall m. Monad m => T MyStateMachine (MkStateName "off") (MkActionName "turnOn") m
-- control_off_turnOn = mkControl f
--   where
--   f set _ st = do
--     set (\s -> s)
--     pure unit

-- control =
--   { on:
--       { turnOff: control_on_turnOff
--       }
--   , off:
--       { turnOn: control_off_turnOn
--       }
--   }

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
