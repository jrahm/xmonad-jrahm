{-# LANGUAGE ScopedTypeVariables #-}
module Internal.Marking where

import XMonad
import XMonad.StackSet hiding (focus)
import Data.IORef
import Data.Map (Map)

import System.FilePath
import System.IO
import Control.Exception

import qualified Data.Map as Map

{- Submodule that handles marking windows so they can be jumped back to. -}

type Mark = Char

data MarkState =
  MarkState {
    markStateMap :: Map Mark Window 
  , markLast :: Maybe Window
  } deriving (Read, Show)

data MarkContext = MarkContext (IORef MarkState)

readMarkState :: IO MarkState
readMarkState = do
  dir <- getXMonadDir
  let markstate = dir </> "markstate"
  catch
    (read <$> (hGetContents =<< openFile markstate ReadMode))
    (\(e :: IOError) -> return (MarkState mempty Nothing))

saveMarkState :: MarkState -> X ()
saveMarkState ms = do
  dir <- getXMonadDir
  let markstate = dir </> "markstate"
  liftIO $ writeFile markstate (show ms)


withNewMarkContext :: (MarkContext -> IO a) -> IO a
withNewMarkContext fn = do
  ioref <- newIORef =<< readMarkState
  fn (MarkContext ioref)

markCurrentWindow :: MarkContext -> Mark -> X ()
markCurrentWindow (MarkContext ioref) mark = do
  withFocused $ \win ->
    liftIO $
      modifyIORef ioref $ \state@(MarkState {markStateMap = ms}) ->
        state {
          markStateMap = Map.insert mark win ms
        }

  saveMarkState =<< liftIO (readIORef ioref)

saveLastMark :: MarkContext -> X ()
saveLastMark (MarkContext ioref) =
  withFocused $ \win -> do
      liftIO $ modifyIORef ioref (\state -> state { markLast = Just win })

jumpToLast :: MarkContext -> X ()
jumpToLast ctx@(MarkContext ioref) = do
  m <- markLast <$> (liftIO $ readIORef ioref)
  saveLastMark ctx
  mapM_ focus m

  saveMarkState =<< liftIO (readIORef ioref)

jumpToMark :: MarkContext -> Mark -> X ()
jumpToMark ctx@(MarkContext ioref) mark = do
  MarkState {markStateMap = m} <- liftIO $ readIORef ioref
  case Map.lookup mark m of
    Nothing -> return ()
    Just w -> do
      saveLastMark ctx
      focus w

  saveMarkState =<< liftIO (readIORef ioref)

mapWindows :: (Ord a, Ord b) => (a -> b) -> StackSet i l a s sd -> StackSet i l b s sd
mapWindows fn (StackSet cur vis hid float) =
  StackSet
    (mapWindowsScreen cur)
    (map mapWindowsScreen vis)
    (map mapWindowsWorkspace hid)
    (Map.mapKeys fn float)
  where
    mapWindowsScreen (Screen work a b) = Screen (mapWindowsWorkspace work) a b
    mapWindowsWorkspace (Workspace t l stack) =
      Workspace t l (fmap (mapStack fn) stack)

-- | What genius decided to hide the instances for the Stack type!!???
mapStack :: (a -> b) -> Stack a -> Stack b
mapStack fn (Stack focus up down) = Stack (fn focus) (map fn up) (map fn down)

setFocusedWindow :: a -> StackSet i l a s sd -> StackSet i l a s sd
setFocusedWindow
  window
  (StackSet (Screen (Workspace t l stack) a b) vis hid float) =
    let newStack =
          case stack of
            Nothing -> Nothing
            Just (Stack _ up down) -> Just (Stack window up down) in
    (StackSet (Screen (Workspace t l newStack) a b) vis hid float)

swapWithFocused :: (Ord a) => a -> StackSet i l a s sd -> StackSet i l a s sd
swapWithFocused winToSwap stackSet =
  case peek stackSet of
    Nothing -> stackSet
    Just focused -> do
       setFocusedWindow winToSwap $
          mapWindows (
            \w -> if w == winToSwap then focused else w) stackSet

swapWithLastMark :: MarkContext -> X ()
swapWithLastMark ctx@(MarkContext ioref) = do
  MarkState {markStateMap = m} <- liftIO $ readIORef ioref
  m <- markLast <$> (liftIO $ readIORef ioref)
  saveLastMark ctx

  case m of
    Nothing -> return ()
    Just win -> windows $ swapWithFocused win

swapWithMark :: MarkContext -> Mark -> X ()
swapWithMark ctx@(MarkContext ioref) mark = do
  MarkState {markStateMap = m} <- liftIO $ readIORef ioref
  saveLastMark ctx

  case Map.lookup mark m of
    Nothing -> return ()
    Just winToSwap ->
      windows $ swapWithFocused winToSwap
