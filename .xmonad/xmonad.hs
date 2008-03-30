import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys (XConfig {modMask = modm}) =
    M.fromList $
         [ ((modm,               xK_Up    ), windows W.focusUp)
         , ((modm,               xK_Down  ), windows W.focusDown)
         , ((modm,               xK_Right ), sendMessage (IncMasterN 1))
         , ((modm,               xK_Left  ), sendMessage (IncMasterN (-1)))
         , ((modm,               xK_period), nextWS )
         , ((modm,               xK_comma ), prevWS )
         , ((modm .|. shiftMask, xK_comma ), shiftToPrev )
         , ((modm .|. shiftMask, xK_period), shiftToNext )]

main = xmonad $ defaultConfig 
       { manageHook       = manageDocks <+> manageHook defaultConfig
       , logHook          = ewmhDesktopsLogHook
       , layoutHook       = avoidStruts $ layoutHook defaultConfig
       , modMask          = mod4Mask
       , keys             = \c -> myKeys c `M.union` keys defaultConfig c
       }
