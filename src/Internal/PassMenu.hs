module Internal.PassMenu where

import XMonad
import XMonad.Util.Run
import Control.Monad

runPassMenu :: X ()
runPassMenu = void $
  safeSpawn "passmenu" [
    "-p", "Password ", 
    "-l", "12",
    "-dim", "0.4",
    "-sb", "#f54245",
    "-nf", "#f54245" ]
    
