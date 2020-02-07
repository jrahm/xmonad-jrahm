{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Internal.Layout where 

import XMonad.Layout.Circle
import XMonad.Layout.Accordion
import Control.Applicative
import XMonad.Layout.Spacing
import Data.List
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Grid
import XMonad.Layout
import XMonad.Layout.LayoutModifier
import XMonad
import XMonad.Core

import qualified XMonad.StackSet as W

myLayout =
  ModifiedLayout (Zoomable False 0.05 0.05) $
    spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True $
      spiral (6/7) |||
      Tall 1 (3/100) (1/2) |||
      ThreeCol 1 (3/100) (1/2) |||
      Circle |||
      Accordion ||| 
      Grid

data ResizeZoom = ShrinkZoom | ExpandZoom deriving (Typeable)

instance Message ResizeZoom where

data Zoomable a = Zoomable Bool Float Float -- True if zooming in on the focused window.
  deriving (Show, Read)

data ToggleZoom = ToggleZoom
  deriving (Typeable)

data AlterZoom = AlterZoom Float Float
  deriving (Typeable)

instance Message ToggleZoom where

instance Message AlterZoom where

instance (Eq a) => LayoutModifier Zoomable a where
  redoLayout (Zoomable doit ws hs) (Rectangle x y w h) stack returned =
    if doit
      then
        let focused = W.focus <$> stack
            (zoomed, rest) = partition ((==focused) . Just . fst) returned
         in case zoomed of
              [] -> return (rest, Nothing)
              ((fwin, _):_) ->  return $ ((fwin, Rectangle (x + wp) (y + hp) (w - fromIntegral (wp * 2)) (h - fromIntegral (hp * 2))) : rest, Nothing)

      else return (returned, Nothing)
    where
      wp = floor $ (fromIntegral w) * ws
      hp = floor $ (fromIntegral h) * hs

  handleMessOrMaybeModifyIt self@(Zoomable showing sw sh) mess =
    return $
      (handleResize <$> fromMessage mess)
        <|> ((Left . handleZoom) <$> fromMessage mess)
      where
        handleResize r =
          if showing
            then Left $ Zoomable showing (guard $ sw + d) (guard $ sh + d)
            else Right $ case r of
                   ShrinkZoom -> SomeMessage Shrink
                   ExpandZoom -> SomeMessage Expand

          where d = (case r of
                      ShrinkZoom -> -1
                      ExpandZoom -> 1) * 0.02

        handleZoom ToggleZoom = Zoomable (not showing) sw sh

        guard f | f > 1 = 1
                | f < 0 = 0
                | otherwise = f
