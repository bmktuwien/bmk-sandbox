import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import XMonad.Actions.OnScreen
import XMonad.Config.Gnome

myManageHook = composeAll
    [ manageHook gnomeConfig
    , className =? "Unity-2d-panel" --> doIgnore
    , className =? "Google-chrome"  --> doShift "1"
    , className =? "Mplayer"        --> doFloat
    ]

main = do
  xmonad $ gnomeConfig
      { manageHook = myManageHook
      , modMask = mod4Mask     -- Rebind Mod to the Windows key
      } `additionalKeys` myKeys


myWorkspaces = ["1","2","3","4","5","6","7","8"]

myKeys =
      [
        -- other additional keys
        ((mod4Mask,  xK_p ), spawn "dmenu_run")
      , ((mod4Mask,  xK_g ), spawn "google_chrome")
      , ((mod4Mask,  xK_e ), spawn "emacs")
      , ((mod4Mask,  xK_t ), spawn "thunderbird")
      ]
      ++
      [((m .|. mod4Mask, k), windows $ f i)
      | (i, k) <- zip myWorkspaces [xK_1 .. xK_8]
      , (f, m) <- [(viewOnScreen 0, 0)
                  ]
      ]
