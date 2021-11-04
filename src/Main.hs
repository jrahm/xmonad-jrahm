import XMonad

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad (when)
import Control.Monad.Writer
import Data.Ord
import Data.List (partition, isPrefixOf, sortBy)
import Data.List.Split
import Data.Maybe
import Internal.Keys
import Internal.Layout
import Internal.LayoutDraw
import System.Directory
import System.FilePath
import System.IO
import System.Process
import Text.Printf
import XMonad.Actions.WindowNavigation
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Spacing
import XMonad.Util.CustomKeys
import XMonad.Util.NamedWindows
import XMonad.Util.Run (spawnPipe)

import qualified XMonad.StackSet as S

data WorkspaceState = Current | Hidden | Visible

getWorkspaces :: (Ord i) => S.StackSet i l a sid sd -> [(WorkspaceState, i)]
getWorkspaces (S.StackSet (S.Screen cur _ _) vis hi _) =
  sortBy (comparing snd) $
    mapMaybe (\(a, S.Workspace t _ s) -> fmap (const (a, t)) s) $
      map (\w -> (Hidden, w)) hi ++
        map (\(S.Screen w _ _) -> (Visible, w)) vis ++
          [(Current, cur)]


main = do
  -- Execute some commands.
  homeDir <- getHomeDirectory
  let fp = homeDir </> ".xmonad" </> "startup"

  xmproc <- spawnPipe "xmobar"
  hSetEncoding xmproc utf8

  logFile <- openFile "/tmp/xmonad.log" WriteMode 

  hPutStrLn logFile "·······························"
  hFlush logFile

  config <-
    applyKeys $ def
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

           winset <- gets windowset
           title <- maybe (pure "") (fmap show . getName) . S.peek $ winset
           let wss = getWorkspaces winset

           liftIO $ do
             hPutStrLn xmproc $ trunc 80 $ execWriter $ do
               mapM_ tell layout
               tell $ xmobarColor "#404040" "" " │ "

               forM_ wss $ \(t, name) -> do
                 case t of
                   Current -> tell "<fn=1><fc=#ff8888>"
                   Visible -> tell "<fn=6><fc=#8888ff>"
                   Hidden -> tell "<fn=2><fc=#888888>"
                 tell name
                 tell " </fc></fn>"

               tell $ xmobarColor "#404040" "" "│ "
               tell $ "<fc=#808080><fn=3>"
               tell $ title
               tell $ "</fn></fc>"
       }

  -- let toggleStructsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

  xmonad (docks config)

  where
    trunc amt str = trunc' False amt str

    trunc' :: Bool -> Int -> String -> String
    trunc' _ _ [] = []
    trunc' ignore amt (a:as) =
      case a of
        '<' -> a : trunc' True amt as
        '>' -> a : trunc' False amt as
        _ ->
          if ignore
            then a : trunc' True amt as
            else
              case amt of
                0 -> trunc' False 0 as
                3 -> "..." ++ trunc' False 0 as
                _ ->  a : trunc' False (amt - 1) as

    splitOnAll arr str = splitOnAll' arr [str]
    splitOnAll' [] str = str
    splitOnAll' (a:as) [str] = splitOnAll' as (splitOn a str)
    splitOnAll' _ lst = lst

