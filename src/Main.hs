import XMonad

-- import Control.Concurrent
-- import Control.Exception
-- import Control.Monad
-- import Control.Monad (when)
-- import Control.Monad.Writer
-- import Data.Ord
-- import Data.List (partition, isPrefixOf, sortBy)
-- import Data.List.Split
-- import Data.Maybe
-- import Internal.Keys
-- import Internal.Layout
-- import Internal.LayoutDraw
-- import Internal.XMobarLog
-- import System.Directory
-- import System.FilePath
-- import System.IO
-- import System.Process
-- import Text.Printf
-- import XMonad.Actions.WindowNavigation
-- import XMonad.Hooks.DynamicLog
-- import XMonad.Hooks.EwmhDesktops
-- import XMonad.Hooks.ManageDocks
-- import XMonad.Hooks.ManageHelpers
-- import XMonad.Layout.IndependentScreens
-- import XMonad.Layout.Spacing
-- import XMonad.Util.CustomKeys
-- import XMonad.Util.NamedWindows
-- import XMonad.Util.Run (spawnPipe)

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
       , keys = \config -> mempty
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
