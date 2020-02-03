{-# LANGUAGE ScopedTypeVariables #-}
module Internal.Marking where

import XMonad
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
