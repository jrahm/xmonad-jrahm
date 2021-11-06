module Internal.XMobarLog ( XMobarLog, spawnXMobar, xMobarLogHook ) where

import Control.Arrow (second)
import Control.Monad (forM_)
import Control.Monad.Writer (tell, execWriter)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Internal.LayoutDraw (drawLayout)
import System.IO (Handle, hSetEncoding, hPutStrLn, utf8)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (spawnPipe)
import XMonad (X)
import Internal.Lib (getPopulatedWorkspaces, WorkspaceState(..))

import qualified XMonad as X
import qualified XMonad.StackSet as S

data XMobarLog = XMobarLog Handle

-- The log hook for XMobar. This is a custom log hook that does not use any
-- of the Xmonad dynamic log libraries.
--
-- This is because the given dynamic log libraries don't handle unicode properly
-- and this has been causing issues. It is also more flexible and frankly easier
-- to just DIY.

spawnXMobar :: IO XMobarLog
spawnXMobar = do
  pipe <- spawnPipe "xmobar"
  hSetEncoding pipe utf8
  return (XMobarLog pipe)


-- XMonad Log Hook meant to be used with the XMonad config logHook.
xMobarLogHook :: XMobarLog -> X ()
xMobarLogHook (XMobarLog xmproc) = do
  (_, _, layoutXpm) <- drawLayout

  winset <- X.gets X.windowset
  title <- maybe (pure "") (fmap show . getName) . S.peek $ winset
  let wss = getPopulatedWorkspaces winset

  X.liftIO $ do
    hPutStrLn xmproc $ trunc 80 $ execWriter $ do
      tell layoutXpm
      tell $ "<fc=#404040> │ </fc>"

      forM_ wss $ \(t, ws) -> do
        case t of
          Current -> tell "<fn=1><fc=#ff8888>"
          Visible -> tell "<fn=6><fc=#8888ff>"
          Hidden -> tell "<fn=2><fc=#888888>"
        tell (S.tag ws)
        tell " </fc></fn>"

      tell $ "<fc=#404040>│ </fc><fc=#a0a0a0><fn=3>"
      tell $ title
      tell $ "</fn></fc>"

-- Truncate an XMobar string to the provided number of _visible_ characters.
-- This is to keep long window titles from overrunning the whole bar.
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
