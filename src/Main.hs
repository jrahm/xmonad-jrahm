import XMonad
import Control.Exception
import XMonad.Hooks.DynamicLog
import Control.Concurrent
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
import Data.List.Split
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import Control.Monad (when)
import System.IO

import Internal.Keys
import Internal.LayoutDraw
import Data.List (partition, isPrefixOf)
import Data.Maybe


main = do
  -- Execute some commands.
  homeDir <- getHomeDirectory
  let fp = homeDir </> ".xmonad" </> "startup"

  xmproc <- spawnPipe "xmobar"

  config <-
    applyKeys $ docks $ def
       { terminal    = "alacritty"
       , modMask     = mod3Mask
       , borderWidth = 2
       , keys = \config -> mempty
       , focusedBorderColor = "#ff6c00"
       -- , normalBorderColor = "#ffd9bf"
       , normalBorderColor = "#000000"
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
       , logHook = do
           (_, _, layout) <- showLayout

           dynamicLogWithPP $ xmobarPP {
               ppCurrent = xmobarColor "#ff8888" "red" . printf "<fn=1>%s</fn>"
             , ppVisible = xmobarColor "#8888ff" "" . printf "<fn=6>%s</fn>"
             , ppHidden  = xmobarColor "#888888" "" . printf "<fn=2>%s</fn>"
             , ppWsSep = "<fn=1><fc=#808080> </fc></fn>"
             , ppTitle =
                 xmobarColor "#808080" "" .
                    printf "<fn=3><fc=#bbbbbb>%s</fc></fn>" .
                      parseOut .
                        trunc 50

             , ppSep = xmobarColor "#404040" "" " │ "
             , ppLayout = const (fromMaybe "" layout)
             , ppExtras = []
             , ppOutput = hPutStrLn xmproc
             , ppOrder =  \ss ->
                 let (icons, etc) = partition ("<icon"`isPrefixOf`) ss
                     in icons ++ etc
             }
       }

  -- let toggleStructsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

  xmonad config

  where
    parseOut :: String -> String
    parseOut str =
      let colors = ["#ff878f", "#e187ff", "#8f87ff", "#87fff7", "#8bff87", "#ffe987", "#ff8787"]
          components = zip (cycle colors) (splitOnAll [" - ", " | ", " · ", " "] str)
          in  concatMap (\(color, str) ->
                  printf "<fc=%s>%s</fc> " color str) components

    trunc amt str =
      if length str > amt - 4
        then take (amt - 4) str ++ " ..."
        else str

    splitOnAll arr str = splitOnAll' arr [str]
    splitOnAll' [] str = str
    splitOnAll' (a:as) [str] = splitOnAll' as (splitOn a str)
    splitOnAll' _ lst = lst

