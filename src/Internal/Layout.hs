{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Internal.Layout where 

import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Grid
import XMonad.Layout
import XMonad

import qualified XMonad.StackSet as W

myLayout =
    spiral (6/7) |||
    Center 0.7 |||
    Tall 1 (3/100) (1/2) |||
    ThreeCol 1 (3/100) (1/2) |||
    Grid


data Center a =
  Center {
    proportion :: Float -- between 0 and 1
  }
  deriving (Show, Read)

instance (Show a) => LayoutClass Center a where
  doLayout l r@(Rectangle x y w h) stack = do
    let wf = fromIntegral w
        hf = fromIntegral h
        x' = (wf - wf * proportion l) / 2
        y' = (hf - hf * proportion l) / 2
        w' = wf * proportion l
        h' = hf * proportion l
        middleRect = Rectangle (floor x') (floor y') (floor w') (floor h')
        topRect = Rectangle 0 0 (floor wf) (floor y')
        rightRect = Rectangle (floor (x' + w')) (floor y') (floor x') (floor h')
        bottomRect = Rectangle 0 (floor $ y' + h') (floor wf) (floor y')
        leftRect = Rectangle 0 (floor y') (floor x') (floor h')

        nWin = length (W.integrate stack)
        winsTop = nWin `div` 8

        portion = fromIntegral $ (guard 1 (nWin `div` 6))
        winRem = fromIntegral $ nWin `mod` 6
      in do
      let ret =
            (zip (W.integrate stack) (
              (:) middleRect $
                 (divRect topRect (portion * 2))
                 ++ (divRect rightRect portion)
                 ++ (divRect bottomRect (portion * 2))
                 ++ (divRect leftRect (portion + winRem))), Just l)
      return ret
   where
    guard n 0 = n
    guard _ n = n

    divRect (Rectangle x y w h) n =
      if h > w
        then
          let h' = h `div` n
            in flip map [0..(n - 1)] $ \mul ->
              Rectangle x (y + fromIntegral (h' * mul)) w h'
        else
          let w' = w `div` n
            in flip map [0..(n - 1)] $ \mul ->
              Rectangle (x + fromIntegral (w' * mul)) y w' h

  handleMessage (Center prop) m =
    return $ fmap resize (fromMessage m)
    where
      resize Shrink = (Center (prop - 0.05))
      resize Expand = (Center (prop + 0.05))
    

  emptyLayout c root = return ([], Just c)
