{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- Creates a layout, the "corner layout" that keeps the master window in the
-- corner and the other windows go around it.
module Internal.CornerLayout where

import Data.Typeable (Typeable)
import XMonad (LayoutClass(..), Rectangle(..), Resize(..), fromMessage)
import qualified XMonad.StackSet as S

data Corner a = Corner Rational Rational
  deriving (Show, Typeable, Read)

instance LayoutClass Corner a where
  pureLayout (Corner frac _) screen@(Rectangle x y w h) ss =
    let w' = floor $ fromIntegral w * frac
        h' = floor $ fromIntegral h * frac
        corner = Rectangle 0 0 w' h'
        vertRect = Rectangle (fromIntegral w') 0 (w - w') h
        horizRect = Rectangle 0 (fromIntegral h') w' (h - h')
        ws = S.integrate ss

        vn = (length ws - 1) `div` 2
        hn = (length ws - 1) - vn
      in 
        case ws of
          [a] -> [(a, screen)]
          [a, b] -> [
            (a, Rectangle x y w' h),
            (b, Rectangle (x + fromIntegral w') y (w - w') h)]
          _ -> 
            zip ws $ map (
                \(Rectangle x' y' w h) -> Rectangle (x + x') (y + y') w h) $ 
              corner :
                (splitVert vertRect vn) ++
                (splitHoriz horizRect hn)

  pureMessage (Corner frac delta) m = fmap resize (fromMessage m)
    where
      resize Shrink = Corner (frac - delta) delta
      resize Expand = Corner (frac + delta) delta

splitVert :: Rectangle -> Int -> [Rectangle]
splitVert (Rectangle x y w h) i' =
  map
    (\i -> Rectangle x (y + fromIntegral (step * i)) w step)
    [0 .. i - 1]
  where
    i = fromIntegral i'
    step = h `div` i

splitHoriz :: Rectangle -> Int -> [Rectangle]
splitHoriz (Rectangle x y w h) i' =
  map
    (\i -> Rectangle (x + fromIntegral (step * i)) y step h)
    [0 .. i - 1]
  where
    step = w `div` i
    i = fromIntegral i'
