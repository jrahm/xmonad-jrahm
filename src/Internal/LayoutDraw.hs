{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
ScopedTypeVariables, BangPatterns #-}
module Internal.LayoutDraw where

import System.IO

import Control.Monad.Writer
import XMonad.Layout.Spacing
import System.Process
import Text.Printf
import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Concurrent (threadDelay)

import System.FilePath
import XMonad
import XMonad.StackSet as S
import Data.Maybe
import Data.Foldable
import System.Directory

import Internal.Layout
import Internal.Hash

showLayout :: X (Bool, String, Maybe String)
showLayout = do
    winset <- gets windowset
    let layout = S.layout . S.workspace . S.current $ winset

    layout' <- handleMessage layout (
       SomeMessage $ ModifyWindowBorder (
          const (Border 0 0 0 0)))

    let layout'' = layout'

    (cached, xpm) <-
        case layout'' of
          Just l -> drawXpmIO l
          Nothing -> drawXpmIO layout
    return $ (cached, description layout, Just $ printf "<icon=%s/>" xpm)

pointInRect :: (Dimension, Dimension) -> Rectangle -> Bool
pointInRect (x, y) (Rectangle x' y' w h) =
  x <= (fi x' + fi w) && x >= fi x' && y <= (fi y' + fi h) && y >= fi y'
  where
    fi :: (Integral a, Num b) => a -> b
    fi = fromIntegral

sf :: (Integral a) => a
sf = 1024

drawXpmIO :: (LayoutClass layout Window) => layout Window -> X (Bool, String)
drawXpmIO l = do
  dir <- getXMonadDir

  let shrinkAmt = 4

  let (w, h) = (56 + shrinkAmt, 28 + shrinkAmt)
  let descr = description l
  let iconCacheDir = dir </> "icons" </> "cache"
  let iconPath = iconCacheDir </> (quickHash descr ++ ".xpm")

  let colors = [
        "#cc9a9a", "#cc9999", "#cc8080", "#cc6666",
        "#cc4c4c", "#cc3232", "#cc1818", "#cc0000" ]

  (rects', _) <-
    runLayout
      (Workspace "0" l (differentiate [1 .. 7]))
      (Rectangle 0 0 (w * sf) (h * sf))

  let rects = flip map rects' $ \(_, (Rectangle x y w h)) ->
                Rectangle (x `div` sf) (y `div` sf) (w `div` sf) (h `div` sf)

  liftIO $ do
    exists <- doesFileExist iconPath
    createDirectoryIfMissing True iconCacheDir

    when (not exists) $ do
      let xpmText = drawXpm (w, h) (zip (cycle colors) rects) 4
      writeFile iconPath xpmText

    return (exists, iconPath)

drawXpm :: (Dimension, Dimension) -> [(String, Rectangle)] -> Dimension -> String
drawXpm (w, h) rects' shrinkAmt = execWriter $ do
    tell "/* XPM */\n"
    tell "static char *out[] = {\n"
    forM_ rects' $ \(_, rect) -> do
      tell $ "/* " ++ show rect ++ " */\n"
    tell $ "/* --------------------------- */\n"
    forM_ rects $ \(_, rect) -> do
      tell $ "/* " ++ show rect ++ " */\n"
      
    tell $ printf "\"%d %d %d 1 \",\n" (w - shrinkAmt) (h - shrinkAmt) (length rects + 1)

    let zipRects = (zip ['A' .. 'Z'] rects)

    forM_ zipRects $ \(char, (color, _)) -> do
      tell $ printf "\"%c c %s\",\n" char color
    tell "\"% c None\"a,\n"

    forM_ [0 .. h - 1 - shrinkAmt] $ \y -> do
      tell "\""
      forM_ [0 .. w - 1 - shrinkAmt] $ \x -> 
        (case find (matches x y) zipRects of
          Nothing -> tell "%"
          Just (chr, _) -> tell [chr])
      tell "\""
      when (y /= h - 1 - shrinkAmt) (tell ",")
      tell "\n"
    tell "};"

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
