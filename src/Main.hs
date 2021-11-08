import XMonad

import XMonad.Hooks.ManageDocks (docks)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsStartup)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Layout.Fullscreen (fullscreenEventHook)

import Internal.XMobarLog
import Internal.Keys
import Internal.Layout

import qualified XMonad as X
import qualified XMonad.StackSet as S

main = do
  -- Execute some commands.
  homeDir <- getHomeDirectory
  let fp = homeDir </> ".xmonad" </> "startup"

  xmobar <- spawnXMobar

  (=<<) X.xmonad $
    applyKeys $ docks $ def
       { terminal    = "alacritty"
       , modMask     = mod3Mask
       , borderWidth = 2
       , keys = const mempty
       , focusedBorderColor = "#ff6c00"
       , normalBorderColor = "#404040"
       , layoutHook = myLayout
       , startupHook = do
          ewmhDesktopsStartup
          spawn fp  
       , manageHook = composeAll [
           isFullscreen --> doFullFloat
         , className =? "Tilda" --> doFloat
         , className =? "yakuake" --> doFloat
         , className =? "MPlayer" --> doFloat
         , title =? "Event Tester" --> doFloat
         , className =? "mpv" --> doFloat
         , className =? "gnubby_ssh_prompt" --> doFloat
         ]
       , workspaces = map return (['0'..'9'] ++ ['a'..'z'])
       , handleEventHook = fullscreenEventHook
       , focusFollowsMouse = False
       , clickJustFocuses = False
       , logHook = xMobarLogHook xmobar
       }
