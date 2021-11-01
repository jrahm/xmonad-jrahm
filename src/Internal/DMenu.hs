module Internal.DMenu where

import XMonad.Util.Dmenu
import XMonad
import XMonad.Util.Run
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import XMonad.Util.Run
import Data.List (intercalate)

data Colors =
  Colors {
    fg :: String,
    bg :: String
  }  | DefaultColors

runDMenu :: X ()
runDMenu = void $
  safeSpawn "/usr/bin/dmenu_run" [
    "-p", "Execute ", "-l", "12", "-dim", "0.4"]

runDMenuPrompt :: String -> Maybe String -> [String] -> X String
runDMenuPrompt prompt color select =
  let realColor = maybe [] (\c -> ["-sb", c, "-nf", c]) color
       in
    runProcessWithInput "/home/rahm/.local/bin/dmenu_debug.sh" ([
      "-p", prompt,
      "-l", "12",
      "-dim", "0.4" ]  ++ realColor) (intercalate "\n" select)


runDMenuPromptWithMap :: String -> Maybe String -> Map String a -> X (Maybe a)
runDMenuPromptWithMap prompt color map = do
  let realColor = maybe [] (\c -> ["-sb", c, "-nf", c]) color
  menuMapArgs "dmenu"([
      "-p", prompt,
      "-l", "12",
      "-dim", "0.4" ]  ++ realColor) map
