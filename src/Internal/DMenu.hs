module Internal.DMenu where

import XMonad
import XMonad.Util.Run
import Control.Monad

runDMenu :: X ()
runDMenu = void $ do
  safeSpawn "/usr/bin/dmenu_run" [
    "-p", "Execute ", "-l", "12", "-dim", "0.4"]
