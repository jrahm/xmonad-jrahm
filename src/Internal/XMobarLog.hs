module Internal.XMobarLog ( XMobarLog, spawnXMobar, xMobarLogHook ) where

import Control.Monad (forM_)
import Control.Monad.Writer (tell, execWriter)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Internal.LayoutDraw (showLayout)
import System.IO (Handle, hSetEncoding, hPutStrLn, utf8)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (spawnPipe)
import XMonad (X)

import qualified XMonad as X
import qualified XMonad.StackSet as S

data WorkspaceState = Current | Hidden | Visible

data XMobarLog = XMobarLog Handle

-- The log hook for XMobar. This is a custom log hook that does not use any
-- of the Xmonad dynamic log libraries.

spawnXMobar :: IO XMobarLog
spawnXMobar = do
  pipe <- spawnPipe "xmobar"
  hSetEncoding pipe utf8
  return (XMobarLog pipe)

xMobarLogHook :: XMobarLog -> X ()
xMobarLogHook (XMobarLog xmproc) = do
  (_, _, layout) <- showLayout

  winset <- X.gets X.windowset
  title <- maybe (pure "") (fmap show . getName) . S.peek $ winset
  let wss = getWorkspaces winset

  X.liftIO $ do
    hPutStrLn xmproc $ trunc 80 $ execWriter $ do
      mapM_ tell layout
      tell $ "<fc=#404040> │ </fc>"

      forM_ wss $ \(t, name) -> do
        case t of
          Current -> tell "<fn=1><fc=#ff8888>"
          Visible -> tell "<fn=6><fc=#8888ff>"
          Hidden -> tell "<fn=2><fc=#888888>"
        tell name
        tell " </fc></fn>"

      tell $ "<fc=#404040>│ </fc><fc=#a0a0a0><fn=3>"
      tell $ title
      tell $ "</fn></fc>"

trunc :: Int -> String -> String
trunc amt str = reverse $ trunc' False amt str []
  where
    trunc' _ _ [] acc = acc
    trunc' ignore amt (a:as) acc =
      case a of
        '<' -> trunc' True amt as (a : acc)
        '>' -> trunc' False amt as (a : acc)
        _ ->
          if ignore
            then trunc' True amt as (a : acc)
            else
              case amt of
                0 -> trunc' False 0 as acc
                3 -> trunc' False 0 as ("..." ++ acc)
                _ -> trunc' False (amt - 1) as (a : acc)

getWorkspaces :: (Ord i) => S.StackSet i l a sid sd -> [(WorkspaceState, i)]
getWorkspaces (S.StackSet (S.Screen cur _ _) vis hi _) =
  sortBy (comparing snd) $
    mapMaybe (\(a, S.Workspace t _ s) -> fmap (const (a, t)) s) $
      map (\w -> (Hidden, w)) hi ++
        map (\(S.Screen w _ _) -> (Visible, w)) vis ++
          [(Current, cur)]
