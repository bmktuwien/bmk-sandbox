import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import System.IO
import XMonad.Actions.OnScreen

myManageHook = composeAll
    [ className =? "Mplayer"        --> doFloat
    , className =? "Nautilus"       --> doShift "7"
    , className =? "Google-chrome"  --> doShift "1"
    , className =? "Skype"          --> doShift "3"
    , className =? "Pidgin"         --> doShift "3"
    ]

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaultConfig
      { manageHook = manageDocks <+> myManageHook -- make sure to include myManageHook definition from above
                     <+> manageHook defaultConfig
      , layoutHook = avoidStruts  $  layoutHook defaultConfig
      , logHook = dynamicLogWithPP $ xmobarPP
                       { ppOutput = hPutStrLn xmproc
                       , ppTitle  = xmobarColor "green" "" . shorten 50
                       }
      , modMask = mod4Mask     -- Rebind Mod to the Windows key
      } `additionalKeys` myKeys


myWorkspaces1 = ["1","2","3","4"]
myWorkspaces2 = ["5","6","7","8"]

myKeys =
      [
        -- other additional keys
      ]
      ++
      [((m .|. mod4Mask, k), windows $ f i)
           | (i, k) <- zip myWorkspaces1 [xK_1 .. xK_4]
           , (f, m) <- [(viewOnScreen 0, 0)
                       ]
           ]
      ++
      [((m .|. mod4Mask, k), windows $ f i)
           | (i, k) <- zip myWorkspaces2 [xK_5 .. xK_8]
           , (f, m) <- [(viewOnScreen 1, 0)
                       ]
           ]

