import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
import XMonad.Prompt
import XMonad.Prompt.Shell
--import XMonad.Actions.UpdatePointer
import XMonad.Actions.MouseGestures

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S

--myMouse :: XConfig t -> M.Map (ButtonMask, Button) (X ())
myMouse (XConfig {modMask = modm}) = 
    M.fromList $
         [ -- Mouse gestures
           ((modm, button3), mouseGesture gestures) 
         ]

myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys (XConfig {modMask = modm}) =
    M.fromList $
         [ ((modm,               xK_Up    ), windows W.focusUp)
         , ((modm,               xK_Down  ), windows W.focusDown)
         , ((modm .|. shiftMask, xK_Down  ), windows W.swapDown )
         , ((modm .|. shiftMask, xK_Up    ), windows W.swapUp )
         , ((modm,               xK_Right ), sendMessage (IncMasterN 1))
         , ((modm,               xK_Left  ), sendMessage (IncMasterN (-1)))
         , ((modm,               xK_period), nextWS )
         , ((modm,               xK_comma ), prevWS )
         , ((modm .|. shiftMask, xK_comma ), shiftToPrev )
         , ((modm .|. shiftMask, xK_period), shiftToNext )
         , ((modm,               xK_p),      shellPrompt defaultXPConfig )
         -- Search commands
         , ((modm, xK_s), SM.submap $ searchEngineMap 
                            $ S.promptSearchBrowser defaultXPConfig "iceweasel")
         , ((modm .|. shiftMask, xK_s), SM.submap $ searchEngineMap $ S.selectSearch)
         ]

searchEngineMap method = M.fromList $
                         [ ((0, xK_g), method S.google)
                         , ((0, xK_c), method S.codesearch)
                         , ((0, xK_w), method S.wikipedia)
                         ]

gestures = M.fromList $
           [ ([], focus)
           , ([U], \w -> focus w >> windows W.swapUp)
           , ([D], \w -> focus w >> windows W.swapDown)
           , ([R, D], \_ -> sendMessage NextLayout)
           , ([R, D, L, U], \w -> focus w >> kill)
           ]

main = xmonad $ defaultConfig 
       { manageHook        = manageDocks <+> manageHook defaultConfig
       , logHook           = ewmhDesktopsLogHook -- >> updatePointer Nearest
       , layoutHook        = ewmhDesktopsLayout $ avoidStruts $ layoutHook defaultConfig
       , modMask           = mod4Mask
       , keys              = \c -> myKeys c `M.union` keys defaultConfig c
       , mouseBindings     = \c -> myMouse c `M.union` (mouseBindings defaultConfig c)
       , focusFollowsMouse = False
       }
