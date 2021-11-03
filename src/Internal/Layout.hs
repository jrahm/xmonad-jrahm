{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Internal.Layout where 

import Control.Arrow (second)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Circle
import XMonad.Layout.Accordion
import Control.Applicative
import XMonad.Layout.Spacing
import Data.List
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Grid
import XMonad.Layout.Dishes
import XMonad.Layout.MosaicAlt
import qualified XMonad.Layout.Dwindle as D
import XMonad.Layout
import XMonad.Layout.LayoutModifier
import XMonad
import XMonad.Core

import qualified Data.Map as M
import qualified XMonad.StackSet as W

myLayout =
  ModifiedLayout (Zoomable False 0.05 0.05) $
    ModifiedLayout (Flippable False) $
      ModifiedLayout (HFlippable False) $
        spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True $
          spiral (6/7) |||
          ModifyDescription TallDescriptionModifier (Tall 1 (3/100) (1/2)) |||
          ModifyDescription ThreeColDescMod (ThreeCol 1 (3/100) (1/2)) |||
          Full |||
          Grid |||
          Dishes 2 (1/6) |||
          (MosaicAlt M.empty :: MosaicAlt Window) |||
          (D.Dwindle D.R D.CW 1.5 1.1)

data ModifyDescription m l a = ModifyDescription m (l a)
  deriving (Show, Read)

data TallDescriptionModifier = TallDescriptionModifier
  deriving (Show, Read)

data ThreeColDescMod = ThreeColDescMod
  deriving (Show, Read)

class DescriptionModifier m l where
  newDescription :: m -> l a -> String -> String

instance (Show m, DescriptionModifier m l, LayoutClass l a) => LayoutClass (ModifyDescription m l) a where
  runLayout (W.Workspace t (ModifyDescription m l) a) rect = do
    (rects, maybeNewLayout) <- runLayout (W.Workspace t l a) rect
    return (rects, fmap (ModifyDescription m) maybeNewLayout)

  doLayout (ModifyDescription m l) a s = do
    (rects, maybeNewLayout) <- doLayout l a s
    return (rects, fmap (ModifyDescription m) maybeNewLayout)

  pureLayout (ModifyDescription m l) a s = pureLayout l a s

  emptyLayout (ModifyDescription m l) a = do
    (rects, maybeNewLayout) <- emptyLayout l a
    return (rects, fmap (ModifyDescription m) maybeNewLayout)

  handleMessage (ModifyDescription m l) a = do
    maybeNewLayout <- handleMessage l a
    return (ModifyDescription m <$> maybeNewLayout)

  pureMessage (ModifyDescription m l) a =
    let maybeNewLayout = pureMessage l a in
      ModifyDescription m <$> maybeNewLayout

  description (ModifyDescription m l) = newDescription m l (description l)

instance DescriptionModifier TallDescriptionModifier Tall where
  newDescription _ (Tall mast _ _) _ = "Tall(" ++ show mast ++ ")"

instance DescriptionModifier ThreeColDescMod ThreeCol where
  newDescription _ (ThreeCol mast _ _) _ = "ThreeCol(" ++ show mast ++ ")"

data ResizeZoom = ShrinkZoom | ExpandZoom deriving (Typeable)

instance Message ResizeZoom where

data Flippable a = Flippable Bool -- True if flipped
  deriving (Show, Read)

data HFlippable a = HFlippable Bool -- True if flipped
  deriving (Show, Read)

data FlipLayout = FlipLayout deriving (Typeable)

data HFlipLayout = HFlipLayout deriving (Typeable)

data Zoomable a = Zoomable Bool Float Float -- True if zooming in on the focused window.
  deriving (Show, Read)

data ToggleZoom = ToggleZoom
  deriving (Typeable)

instance Message FlipLayout where

instance Message HFlipLayout where

instance Message ToggleZoom where

instance (Eq a) => LayoutModifier Flippable a where
  pureModifier (Flippable flip) (Rectangle sx _ sw _) stack returned =
    if flip 
      then (map (second doFlip) returned, Nothing)
      else (returned, Nothing)
    where
      doFlip (Rectangle x y w h) =
        Rectangle ((sx + fromIntegral sw) - x - fromIntegral w + sx) y w h

  pureMess (Flippable flip) message = 
    case fromMessage message of
      Just FlipLayout -> Just (Flippable (not flip))
      Nothing -> Nothing

  modifyDescription (Flippable flipped) underlying =
    let descr = description underlying in
      if flipped
        then descr ++ " Flipped"
        else descr

instance (Eq a) => LayoutModifier HFlippable a where
  pureModifier (HFlippable flip) (Rectangle _ sy _ sh) stack returned =
    if flip 
      then (map (second doFlip) returned, Nothing)
      else (returned, Nothing)
    where
      doFlip (Rectangle x y w h) =
        Rectangle x ((sy + fromIntegral sh) - y - fromIntegral h + sy) w h

  pureMess (HFlippable flip) message = 
    case fromMessage message of
      Just HFlipLayout -> Just (HFlippable (not flip))
      Nothing -> Nothing

  modifyDescription (HFlippable flipped) underlying =
    let descr = description underlying in
      if flipped
        then descr ++ " HFlipped"
        else descr
    

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
