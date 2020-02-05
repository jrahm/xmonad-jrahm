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
import XMonad.Layout.IndependentScreens
import Text.Printf
import XMonad.Hooks.EwmhDesktops

import Internal.Keys
import Internal.LayoutDraw
import Data.List (partition, isPrefixOf)


main = do
  -- Execute some commands.
  homeDir <- getHomeDirectory
  let fp = homeDir </> ".xmonad" </> "startup"
  let theLayout = myLayout

  config <-
    applyKeys $ def
       { terminal    = "st"
       , modMask     = mod4Mask
       , borderWidth = 0
       , keys = \config -> mempty
       , focusedBorderColor = "#FFFFFF"
       , normalBorderColor = "#000000"
       , layoutHook =
              spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True $
              InterceptLayout $
                myLayout
       , startupHook = do
           spawn fp
       , manageHook = composeAll [
           isFullscreen --> doFullFloat
         , className =? "Tilda" --> doFloat
         , className =? "MPlayer" --> doFloat
         , className =? "mpv" --> doFloat
         , className =? "gnubby_ssh_prompt" --> doFloat
         ]
           , handleEventHook = fullscreenEventHook
       }

  let toggleStructsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

  xmonad =<<
    statusBar
      "xmobar"
      xmobarPP {
                 ppCurrent = xmobarColor "#ffffff" "red" . printf "%s"
               , ppVisible = xmobarColor "#8888ff" "" . printf "%s"
               , ppHidden  = xmobarColor "#888888" "" . printf "%s"
               , ppWsSep = "<fn=1><fc=#808080> · </fc></fn>"
               , ppTitle =
                   xmobarColor "#8888ff" "" . printf "%s" .
                      (printf "<fn=1>%s</fn>" :: String -> String)

               , ppSep = xmobarColor "#404040" "" "   ────   "
               , ppLayout = const ""
               , ppExtras = [showLayout]
               , ppOrder =  \ss ->
                   let (icons, etc) = partition ("<icon"`isPrefixOf`) ss in icons ++ etc
               }
      toggleStructsKey
      config { modMask = mod4Mask }
