module Internal.Keys where

import qualified Data.Map as Map
import Data.Map (Map)
import Internal.Marking
import XMonad.Actions.Submap
import XMonad.Util.CustomKeys
import XMonad
import Control.Monad
import XMonad.Actions.WindowNavigation
import qualified XMonad.StackSet as W

type KeyMap l = XConfig l -> Map (KeyMask, KeySym) (X ())

applyKeys :: XConfig l -> IO (XConfig l)
applyKeys config@(XConfig {modMask = modm}) = do
   ks <- newKeys
   withWindowNavigation (xK_k, xK_h, xK_j, xK_l) $
     config { keys = ks }

newKeys :: IO (KeyMap l)
newKeys =
  withNewMarkContext $ \markContext ->
    return $ \config@(XConfig {modMask = modm}) ->
      let workspacesByInt =
            Map.fromList $
              zip ['1'..] (XMonad.workspaces config)

          gotoWorkspace :: Char -> X ()
          gotoWorkspace ch =
            mapM_ (windows . W.greedyView) (Map.lookup ch workspacesByInt)

          in

      Map.fromList
        [ ((modm, xK_F12), (void $ spawn "spotify-control next"))
        , ((modm, xK_F11), (void $ spawn "spotify-control prev"))
        , ((modm, xK_F10), (void $ spawn "spotify-control play"))
        , ((modm, xK_r),   (void $ spawn "dmenu_run"))
        , ((modm .|. shiftMask, xK_r),   (void $ spawn "gmrun"))
        , ((modm .|. mod1Mask, xK_l), (void $ spawn "xscreensaver-command -lock"))
        , ((modm, xK_t),  (void $ spawn (terminal config)))
        , ((modm, xK_m), (submap $ mapAlpha modm (markCurrentWindow markContext)))
        , ((modm, xK_apostrophe), (submap $
              Map.insert
                (modm, xK_apostrophe)
                (jumpToLast markContext)
                (mapAlpha modm (jumpToMark markContext))))
        , ((modm, xK_g), (submap $ mapNumbers 0 gotoWorkspace))

        , ((modm .|. shiftMask, xK_bracketleft), sendMessage (IncMasterN (-1)))
        , ((modm .|. shiftMask, xK_bracketright), sendMessage (IncMasterN 1))
        , ((modm, xK_bracketleft), sendMessage Shrink)
        , ((modm, xK_bracketright), sendMessage Expand)

        , ((modm, xK_space), sendMessage NextLayout)

        , ((modm, xK_q), spawn "xmonad --recompile && xmonad --restart")
        ]

mapNumbers :: KeyMask -> (Char -> X ()) -> Map (KeyMask, KeySym) (X ())
mapNumbers km fn =
  Map.fromList [
    ((km, xK_0), fn '0')
  , ((km, xK_1), fn '1')
  , ((km, xK_2), fn '2')
  , ((km, xK_3), fn '3')
  , ((km, xK_4), fn '4')
  , ((km, xK_5), fn '5')
  , ((km, xK_6), fn '6')
  , ((km, xK_7), fn '7')
  , ((km, xK_8), fn '8')
  , ((km, xK_9), fn '9')
  ]

mapAlpha :: KeyMask -> (Char -> X ()) -> Map (KeyMask, KeySym) (X ())
mapAlpha km fn =
  Map.fromList [
    ((km, xK_a), fn 'a')
  , ((km, xK_b), fn 'b')
  , ((km, xK_c), fn 'c')
  , ((km, xK_d), fn 'd')
  , ((km, xK_e), fn 'e')
  , ((km, xK_f), fn 'f')
  , ((km, xK_g), fn 'g')
  , ((km, xK_h), fn 'h')
  , ((km, xK_i), fn 'i')
  , ((km, xK_j), fn 'j')
  , ((km, xK_k), fn 'k')
  , ((km, xK_l), fn 'l')
  , ((km, xK_m), fn 'm')
  , ((km, xK_n), fn 'n')
  , ((km, xK_o), fn 'o')
  , ((km, xK_p), fn 'p')
  , ((km, xK_q), fn 'q')
  , ((km, xK_r), fn 'r')
  , ((km, xK_s), fn 's')
  , ((km, xK_t), fn 't')
  , ((km, xK_u), fn 'u')
  , ((km, xK_v), fn 'v')
  , ((km, xK_w), fn 'w')
  , ((km, xK_x), fn 'x')
  , ((km, xK_y), fn 'y')
  , ((km, xK_z), fn 'z')
  ]
