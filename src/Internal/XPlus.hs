module Internal.XPlus where

import Internal.Marking
import XMonad

-- The X Monad with additional information. Used for configuring the system.

data XPlusState l =
    XPlusState {
      markContext :: MarkContext
    , xConfig :: XConfig l
    }

data XPlus l a = XPlus (XPlusState l -> X (a, XPlusState l))

instance Functor (XPlus l) where
  fmap fn (XPlus xfn) =
    XPlus $ \st -> do
      (a, b) <- xfn st
      return (fn a, b)

instance Applicative (XPlus l) where
  pure = return
  (<*>) afn aarg = do
    fn <- afn
    arg <- aarg
    return (fn arg)

instance Monad (XPlus l) where
   -- (>>=) :: XPlus l a -> (a -> XPlus l b) -> XPlus l b
    (>>=) (XPlus afn) bfn = do
       XPlus $ \s0 -> do
         (a, s1) <- afn s0
         let (XPlus xBFn) = bfn a
         xBFn s1

    return x = XPlus $ \s -> return (x, s)

getXPlusState :: XPlus l (XPlusState l)
getXPlusState = XPlus $ \s -> return (s, s)

getXConfig :: XPlus l (XConfig l)
getXConfig = xConfig <$> getXPlusState

getMarkContext :: XPlus l MarkContext
getMarkContext = markContext <$> getXPlusState

runXPlus :: MarkContext -> XConfig l -> XPlus l a -> X a
runXPlus markCtx cfg (XPlus fn) = do
  fst <$> fn (XPlusState markCtx cfg)

liftXPlus :: X a -> XPlus l a
liftXPlus xa = XPlus $ \s -> (\a -> (a, s)) <$> xa
