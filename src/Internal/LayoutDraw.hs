{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
ScopedTypeVariables, BangPatterns #-}
module Internal.LayoutDraw (drawLayout) where

import Control.Monad

import Control.Arrow (second)
import Control.Concurrent (threadDelay)
import Control.Exception (handle)
import Control.Monad.Writer (execWriter, tell)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Internal.Hash (quickHash)
import Internal.Layout (ZoomModifier(..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Text.Printf (printf)
import XMonad.Layout.Spacing (SpacingModifier(..), Border(..))

import XMonad (X,
               Rectangle(..),
               Dimension,
               LayoutClass,
               Message,
               Window,
               SomeMessage(..))

import qualified XMonad as X
import qualified XMonad.StackSet as S

-- Draws and returns an XPM for the current layout.
--
-- Returns
--  - Bool   - true if the xpm has already been written, and is thus cached.
--  - String - description of the current layout
--  - String - the text to send to XMobar
--
-- This function actually runs the current layout's doLayout function to
-- generate the XPM, so it's completely portable to all layouts.
--
-- Note this function is impure and running the layout to create the XPM is also
-- impure. While in-practice most layouts are pure, it should be kept in mind.
drawLayout :: X (Bool, String, String)
drawLayout = do
    winset <- X.gets X.windowset
    let layout = S.layout $ S.workspace $ S.current $ winset

    -- Gotta reset the layout to a consistent state.
    layout' <- foldM (flip ($)) layout $ [
         handleMessage' $ ModifyWindowBorder $ const $ Border 0 0 0 0,
         handleMessage' $ Unzoom
       ]

    (cached, xpm) <- drawXpmIO layout'

    return $ (cached, X.description layout, printf "<icon=%s/>" xpm)

-- Returns true if a point is inside a rectangle (inclusive).
pointInRect :: (Dimension, Dimension) -> Rectangle -> Bool
pointInRect (x, y) (Rectangle x' y' w h) =
  x <= (fi x' + fi w) && x >= fi x' && y <= (fi y' + fi h) && y >= fi y'
  where
    fi :: (Integral a, Num b) => a -> b
    fi = fromIntegral

-- Scale factory. Scaling the rectangles before writing the XPM helps to reduce
-- noise from things like AvoidStruts, as there is unfortunately no way to force
-- avoid struts to be off, one can only toggle it.
sf :: (Integral a) => a
sf = 1024

handleMessage' ::
    (LayoutClass layout a, Message m) => m -> layout a -> X (layout a)
handleMessage' message layout  = do
  fromMaybe layout <$> X.handleMessage layout (SomeMessage message)

-- Creates the XPM for the given layout and returns the path to it.
--
-- This function does run doLayout on the given layout, and that should be
-- accounted for.
drawXpmIO :: (LayoutClass layout Window) => layout Window -> X (Bool, String)
drawXpmIO l = do
  dir <- X.getXMonadDir

  let shrinkAmt = 5 -- amount to shrink the windows by to make pretty gaps.

  let (w, h) = (56, 24)
  let descr = X.description l
  let iconCacheDir = dir </> "icons" </> "cache"
  let iconPath = iconCacheDir </> (quickHash descr ++ ".xpm")

  let colors = [
        "#cc9a9a", "#cc9999", "#cc8080", "#cc6666",
        "#cc4c4c", "#cc3232", "#cc1818", "#cc0000" ]

  (rects', _) <-
    X.runLayout
      (S.Workspace "0" l (S.differentiate [1 .. 5]))
      (Rectangle 0 0 ((w + shrinkAmt) * sf) ((h + shrinkAmt) * sf))

  let rects = flip map rects' $ \(_, (Rectangle x y w h)) ->
                Rectangle (x `div` sf) (y `div` sf) (w `div` sf) (h `div` sf)

  X.liftIO $ do
    exists <- doesFileExist iconPath
    createDirectoryIfMissing True iconCacheDir

    when (not exists) $ do
      let xpmText = drawXpm (w, h) (zip (cycle colors) rects) 4
      writeFile iconPath xpmText

    return (exists, iconPath)

--
-- Create's an XPM, purely. Returns a string with the XPM contents.
-- Takes as arguments
-- 
--   - dimensions of the icon.
--   - list of (color, rectangle) pairs.
--   - The amount to shrink the windows by for those pretty gaps.
--
drawXpm ::
    (Dimension, Dimension) -> [(String, Rectangle)] -> Dimension -> String
drawXpm (w, h) rects' shrinkAmt = execWriter $ do
    tell "/* XPM */\n"
    tell "static char *out[] = {\n"
    tell $ printf "\"%d %d %d 1 \",\n" w h (length rects + 1)

    let zipRects = zip ['A' .. 'Z'] rects

    forM_ zipRects $ \(char, (color, _)) -> do
      tell $ printf "\"%c c %s\",\n" char color
    tell "\"% c None\"a,\n"

    forM_ [0 .. h - 1] $ \y -> do
      tell "\""
      forM_ [0 .. w - 1] $ \x -> 
        (case find (matches x y) zipRects of
          Nothing -> tell "%"
          Just (chr, _) -> tell [chr])
      tell "\""
      when (y /= h - 1 - shrinkAmt) (tell ",")
      tell "\n"
    tell "};\n"

  where
    matches x y (_, (_, r)) = pointInRect (x, y) r
    rects = map (second (shrink shrinkAmt)) rects'
    guard a b = if a <= shrinkAmt then 1 else b
    shrink amt (Rectangle x y w h) =
      Rectangle
        x
        y
        (guard w $ w - fromIntegral amt)
        (guard h $ h - fromIntegral amt)
