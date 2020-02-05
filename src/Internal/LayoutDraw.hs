{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
ScopedTypeVariables, BangPatterns #-}
module Internal.LayoutDraw where

import System.Process
import Text.Printf
import Control.Arrow
import Control.Exception
import Control.Monad
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))

import System.FilePath
import XMonad
import XMonad.StackSet as S
import Data.Maybe
import System.Directory

import Internal.Layout
import Internal.Hash

showLayout :: X (Maybe String)
showLayout = do
    winset <- gets windowset
    let layout = S.layout . S.workspace . S.current $ winset
    xpm <- drawPng layout
    return $ Just $ printf "<icon=%s/>" xpm

iconSize :: (Num a) => (a, a)
iconSize = (64, 32)

drawPng :: (LayoutClass layout Window) => layout Window -> X String
drawPng l = do
    dir <- getXMonadDir
    let sixWindows = [1..(4 :: Window)]
    let stack = differentiate sixWindows
    (rects, _) <-
          runLayout
            (Workspace "0" l stack)
            (Rectangle 0 0 (fst iconSize * 30) (snd iconSize * 30))
    return ()

    let descr = description l
    let pngCacheDir = dir </> "icons" </> "cache"

    liftIO $ createDirectoryIfMissing True pngCacheDir
    let testf = dir </> "text.txt"
    let filepathPng = pngCacheDir </> (quickHash descr ++ ".png")
    let filepathXpm = pngCacheDir </> (quickHash descr ++ ".xpm")

    let colors = [
           -- (1.0, 1.0, 1.0),
           -- (0.9, 0.9, 1.0),
           -- (0.8, 0.8, 1.0),
           -- (0.7, 0.7, 1.0),
           (0.6, 0.6, 0.8),
           (0.5, 0.5, 0.8),
           (0.4, 0.4, 0.8),
           (0.3, 0.3, 0.8),
           (0.2, 0.2, 0.8),
           (0.1, 0.1, 0.8),
           (0.0, 0.0, 0.8)
          ]

    exists <- liftIO $ doesFileExist filepathXpm
    when (not exists) $
      liftIO $ do
        withImageSurface FormatARGB32 64 32 $ \surface -> do
            renderWith surface $ do
              setLineCap LineCapButt
              setLineJoin LineJoinMiter

              forM_ (zip (map (second padR) rects) colors) $
                \((wind, Rectangle x y w h), (r, g, b)) -> do
                  setSourceRGBA r g b 1

                  rectangle
                    (fromIntegral $ floor (fromIntegral x / 30.0))
                    (fromIntegral $ floor (fromIntegral y / 30.0))
                    (fromIntegral $ floor (fromIntegral w / 30.0))
                    (fromIntegral $ floor (fromIntegral h / 30.0))

                  fill

            surfaceWriteToPNG surface filepathPng

            (!_) <- readProcessWithExitCode
              "/usr/bin/convert"
              [filepathPng, filepathXpm]
              ""
            return ()

    return filepathXpm
    where
      padR = id
      -- padR (Rectangle x y w h) =
      --   Rectangle x y (max 1 $ w - 120) (max 1 $ h - 120)

newtype InterceptLayout l a =
    InterceptLayout {
       unIntercept :: (l a)
    } deriving (Show, Read)

instance (LayoutClass l Window) => LayoutClass (InterceptLayout l) Window where
    runLayout (Workspace t l s) rect = do
      (rects, mr) <- runLayout (Workspace t (unIntercept l) s) rect
      return (rects, fmap InterceptLayout mr)

    handleMessage this mesg = do
      ret <- handleMessage (unIntercept this) mesg
      -- mapM_ drawThing ret
      return (InterceptLayout <$> ret)

    description = ("Intercepted "++) . description . unIntercept
