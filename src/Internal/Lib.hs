{-# LANGUAGE RankNTypes #-}
module Internal.Lib where

import Prelude hiding ((!!))

import XMonad.Util.Run
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell

import Internal.PromptConfig

import Data.Char
import Data.List hiding ((!!))
import Data.List.Safe ((!!))
import Data.Maybe
import Internal.Marking
import Internal.XPlus
import Text.Printf
import XMonad hiding (workspaces, Screen)
import XMonad.StackSet hiding (filter, focus)
import qualified Data.Map as Map
import Internal.DMenu
import Data.Ord (comparing)

import qualified XMonad.StackSet as S

type WorkspaceName = Char
newtype Selector = Selector (forall a. (Eq a) => a -> [a] -> a)

data WinPrompt = WinPrompt

instance XPrompt WinPrompt where
    showXPrompt _ = "[Window] "
    commandToComplete _ = id

data WorkspaceState = Current | Hidden | Visible

-- Returns all the workspaces that are either visible, current or Hidden but
-- have windows and that workspace's state.
--
-- In other words, filters out workspaces that have no windows and are not
-- visible.
--
-- This function will sort the result by the workspace tag.
getPopulatedWorkspaces ::
  (Ord i) => S.StackSet i l a sid sd -> [(WorkspaceState, S.Workspace i l a)]
getPopulatedWorkspaces (S.StackSet (S.Screen cur _ _) vis hi _) =
  sortBy (comparing (tag . snd)) $
    mapMaybe (\w@(S.Workspace _ _ s) -> fmap (const (Hidden, w)) s) hi ++
      map (\(S.Screen w _ _) -> (Visible, w)) vis ++
        [(Current, cur)]

getHorizontallyOrderedScreens ::
  StackSet wid l a ScreenId ScreenDetail ->
    [Screen wid l a ScreenId ScreenDetail]
-- ^ Returns a list of screens ordered from leftmost to rightmost.
getHorizontallyOrderedScreens windowSet =
    flip sortBy screens $ \sc1 sc2 ->
      let (SD (Rectangle x1 _ _ _)) = screenDetail sc1
          (SD (Rectangle x2 _ _ _)) = screenDetail sc2
          in x1 `compare` x2
    where
      screens = current windowSet : visible windowSet


gotoWorkspace :: WorkspaceName -> XPlus l ()
gotoWorkspace ch = do
  mc <- getMarkContext
  liftXPlus $ do
    saveLastMark mc
    windows $ greedyView $ return ch

shiftToWorkspace :: WorkspaceName -> XPlus l ()
shiftToWorkspace = liftXPlus . windows . shift . return

swapWorkspace :: WorkspaceName -> XPlus l ()
swapWorkspace toWorkspaceName = liftXPlus $ do
  windows $ \ss -> do
    let fromWorkspace = tag $ workspace $ current ss
        toWorkspace = [toWorkspaceName] in
      (StackSet (swapSc fromWorkspace toWorkspace $ current ss)
                (map (swapSc fromWorkspace toWorkspace) $ visible ss)
                (map (swapWs fromWorkspace toWorkspace) $ hidden ss)
                (floating ss))
  where
    swapSc fromWorkspace toWorkspace (Screen ws a b) =
        Screen (swapWs fromWorkspace toWorkspace ws) a b

    swapWs fromWorkspace toWorkspace ws@(Workspace t' l s)
        | t' == fromWorkspace = Workspace toWorkspace l s
        | t' == toWorkspace = Workspace fromWorkspace l s
        | otherwise = ws

fuzzyCompletion :: String -> String -> Bool
fuzzyCompletion str0 str1 =
  all (`isInfixOf`l0) ws
  where
    ws = filter (not . all isSpace) $ words (map toLower str0)
    l0 = map toLower str1

getString :: Window -> X String
getString = runQuery $ do
  t <- title
  a <- appName
  return $
    if map toLower a `isInfixOf` map toLower t
      then t
      else printf "%s - %s" t a

relativeWorkspaceShift :: Selector -> X ()
relativeWorkspaceShift (Selector selector) = do
  windows $ \ss ->
    let tags = sort $ (tag . snd <$> getPopulatedWorkspaces ss)
        from = tag $ workspace $ current ss
        to = selector from tags
    in greedyView to ss

next :: Selector
next = Selector $ \a l -> select a l l
  where select n (x:y:xs) _ | n == x = y 
        select n [x] (y:_) | n == x = y
        select n (x:xs) orig = select n xs orig
        select n _ _ = n

prev :: Selector
prev = Selector $ \a l ->
  let (Selector fn) = next in fn a (reverse l)

withScreen :: (WorkspaceId -> WindowSet -> WindowSet) -> Int -> XPlus l ()
withScreen fn n = do
  markContext <- getMarkContext

  liftXPlus $
    windows $ \windowSet ->
      case (getHorizontallyOrderedScreens windowSet !! n) of
        Nothing -> windowSet
        Just screen -> fn (tag $ workspace screen) windowSet

windowJump :: XPlus l ()
windowJump = do
  markContext <- getMarkContext

  liftXPlus $ do
    windowTitlesToWinId <- withWindowSet $ \ss ->
      Map.fromList <$> mapM (\wid -> (,) <$> getString wid <*> return wid) (allWindows ss)

    windowId <- runDMenuPromptWithMap "Window" (Just "#f542f5") windowTitlesToWinId

    case windowId of
      Nothing -> return ()
      Just wid -> do
        saveLastMark markContext
        focus wid
    -- mkXPrompt
    --   WinPrompt
    --   xpConfig
    --   (\input -> return $ filter (fuzzyCompletion input) (Map.keys windowTitlesToWinId)) $
    --   \str -> do
    --     saveLastMark markContext
    --     case Map.lookup str windowTitlesToWinId of
    --       Just w -> focus w
    --       Nothing ->
    --         case filter (fuzzyCompletion str) (Map.keys windowTitlesToWinId) of
    --           [s] -> mapM_ focus (Map.lookup s windowTitlesToWinId)
    --           _ -> return ()
