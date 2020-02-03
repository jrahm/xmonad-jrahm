import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spacing
import XMonad.Actions.WindowNavigation
import XMonad.Util.CustomKeys
import System.Directory
import System.FilePath
import System.Process
import Internal.Layout
import XMonad.Hooks.ManageHelpers
import Text.Printf

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
           isFullscreen --> doFloat
         , className =? "Tilda" --> doFloat
         , className =? "MPlayer" --> doFloat
         , className =? "mpv" --> doFloat
         ]
       }
  let toggleStructsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

  xmonad =<<
    statusBar
      "xmobar"
      xmobarPP {
                 ppCurrent = xmobarColor "#ffffff" "red" . printf "%s"
               , ppVisible = xmobarColor "#8888ff" "" . printf "%s"
               , ppHidden  = xmobarColor "#888888" "" . printf "%s"
               , ppWsSep = " · "
               , ppTitle =
                   xmobarColor "#8888ff" "" . printf "%s" .
                      (printf "<fn=1>%s</fn>" :: String -> String)

               , ppSep = xmobarColor "#404040" "" "   ────   "
               }
      toggleStructsKey
      config { modMask = mod4Mask }
