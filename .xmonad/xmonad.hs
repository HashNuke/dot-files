import XMonad

import XMonad.Config.Gnome

main = do
  xmonad $ gnomeConfig
    { terminal    = "gnome-terminal"
    , modMask     = mod4Mask
    , focusFollowsMouse = False
    , borderWidth = 1
    }

