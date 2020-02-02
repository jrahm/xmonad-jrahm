import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spacing
import XMonad.Actions.WindowNavigation
import XMonad.Util.CustomKeys
import System.Directory
import System.FilePath
import System.Process
import Internal.Layout

import Internal.Keys

main = do
  -- Execute some commands.
  homeDir <- getHomeDirectory
  let fp = homeDir </> ".xmonad" </> "startup"

  config <-
    applyKeys $ def
       { terminal    = "st"
       , modMask     = mod4Mask
       , borderWidth = 0
       , keys = \config -> mempty
       , focusedBorderColor = "#FFFFFF"
       , normalBorderColor = "#000000"
       , layoutHook = spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True $
                      myLayout
       , startupHook = do
           spawn fp
       , manageHook = composeAll [
           className =? "Tilda" --> doFloat
         , className =? "MPlayer" --> doFloat
         ]
       }

  xmonad =<< xmobar config { modMask = mod4Mask }
