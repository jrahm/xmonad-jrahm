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
    Tall 1 (3/100) (1/2) |||
    ThreeCol 1 (3/100) (1/2) |||
    Grid

data Manual = Manual RectSet

data RectSet =
      RectSet {
                rectSetWidth :: Float -- 0.0 - 1.0 fraction of the float.
              , rectSetHeight :: Float
              , subRects :: RectChildren
              }

data RectChildren =
      Leaf { boundWindow :: Maybe Window } |
      Children { children :: [RectSet] }

data RectSetD =
    LeafD {
      boundWindowD :: Maybe Window
    , parent :: RectParentD
    } |
    Parent { self :: RectParentD }

data RectParentD =
    RectParentD {
      currentChild :: RectSetD

    , leftChildren :: [RectSetD]
    , rightChildren :: [RectSetD]

    , rectSetDWidth :: Float
    , rectSetDHeight :: Float

    , parentRect :: Maybe RectParentD
    }

derive :: RectSet -> Window -> Maybe RectSetD
derive (RectSet w h sub) = undefined

getWindowRect :: Window -> X (Maybe Rectangle)
getWindowRect win = withDisplay $ \dpy -> do
    (_, x, y, w, h, bw, _) <- liftIO $ getGeometry dpy win
    catchX
      (return $ Just $ Rectangle x y (w + 2 * bw) (h + 2 * bw))
      (return Nothing)
