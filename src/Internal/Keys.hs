{-# LANGUAGE RankNTypes #-}
module Internal.Keys where

import System.Process
import XMonad.Util.Ungrab
import Internal.XPlus
import Data.Maybe (isJust)
import Debug.Trace
import Control.Applicative
import Prelude hiding ((!!))
import Control.Monad
import Data.Char
import Data.List hiding ((!!))
import Data.List.Safe ((!!))
import Data.Map (Map)
import Internal.Layout
import Internal.Marking
import Internal.PromptConfig
import System.IO
import Text.Printf
import XMonad
import XMonad.Actions.Submap
import XMonad.Actions.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell
import XMonad.Util.CustomKeys
import XMonad.Util.Scratchpad
import qualified Data.Map as Map
import qualified XMonad.StackSet as W

import Internal.Lib

type KeyMap l = XConfig l -> Map (KeyMask, KeySym) (X ())

applyKeys :: XConfig l -> IO (XConfig l)
applyKeys config@(XConfig {modMask = modm}) =
  withNewMarkContext $ \markContext -> do
    ks <- newKeys markContext
    ms <- newMouse markContext
    withWindowNavigation (xK_k, xK_h, xK_j, xK_l) $
      config { keys = ks, mouseBindings = ms }

newMouse :: MarkContext -> IO (XConfig l -> Map (KeyMask, Button) (Window -> X ()))
newMouse markContext =
    return $ \config@(XConfig {modMask = modm}) ->
      Map.fromList [
        ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
      , ((modm, button2), windows . (W.shiftMaster .) . W.focusWindow)
      , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)

      , ((modm, 6), const (relativeWorkspaceShift prev))
      , ((modm, 7), const (relativeWorkspaceShift next))

      , ((modm, 8), const (relativeWorkspaceShift prev))
      , ((modm, 9), const (relativeWorkspaceShift next))
      ]

click :: X ()
click = do
  (dpy, root) <- asks $ (,) <$> display <*> theRoot
  (_, _, window, _, _, _, _, _) <- io $ queryPointer dpy root
  focus window

newKeys :: MarkContext -> IO (KeyMap l)
newKeys markContext =
    return $ \config@(XConfig {modMask = modm}) ->
      Map.fromList
        [ ((modm, xK_F12), (void $ spawn "spotify-control next"))
        , ((modm, xK_F11), (void $ spawn "spotify-control prev"))
        , ((modm, xK_semicolon), scratchpadSpawnActionTerminal "scratchpad")
        , ((modm, xK_F10), (void $ spawn "spotify-control play"))
        , ((modm, xK_r),   (void $ spawn "dmenu_run"))
        , ((modm .|. shiftMask, xK_r),   (void $ spawn "gmrun"))
        , ((modm .|. mod1Mask, xK_l), (void $ spawn "xsecurelock"))
        , ((modm .|. mod1Mask, xK_s), (void $ spawn "sudo systemctl suspend"))
        , ((modm .|. shiftMask, xK_c), kill)
        , ((modm .|. shiftMask, xK_t), withFocused $ windows . W.sink)
        , ((modm, xK_t),  (void $ spawn (terminal config)))
        , ((modm, xK_m), (submap $ mapAlpha modm (markCurrentWindow markContext)))
        , ((modm, xK_w), runXPlus markContext config windowJump)
        , ((modm, xK_apostrophe), (submap $
              Map.insert
                (modm, xK_apostrophe)
                (jumpToLast markContext)
                (mapAlpha modm (jumpToMark markContext))))

        , ((modm, xK_g), (submap $
            mapNumbersAndAlpha 0 (
              runXPlus markContext config . gotoWorkspace)))

        , ((modm .|. shiftMask, xK_g), (submap $
            mapNumbersAndAlpha 0 (
              runXPlus markContext config . shiftToWorkspace)))

        , ((modm .|. shiftMask .|. mod1Mask, xK_g), (submap $
            mapNumbersAndAlpha 0 (
              runXPlus markContext config . swapWorkspace)))

        , ((modm .|. shiftMask, xK_bracketleft), sendMessage (IncMasterN (-1)))
        , ((modm .|. shiftMask, xK_bracketright), sendMessage (IncMasterN 1))
        , ((modm, xK_bracketleft), sendMessage ShrinkZoom)
        , ((modm, xK_bracketright), sendMessage ExpandZoom)

        , ((modm, xK_space), sendMessage NextLayout)

        , ((modm, xK_n), relativeWorkspaceShift next)
        , ((modm, xK_p), relativeWorkspaceShift prev)

        , ((modm, xK_q), spawn "xmonad --recompile && xmonad --restart")
        , ((modm, xK_z), sendMessage ToggleZoom)

        , ((modm, xK_Tab), windows W.focusDown)
        , ((modm .|. shiftMask, xK_Tab), windows W.focusUp)

        , ((modm, xK_a), runXPlus markContext config (withScreen W.view 0))
        , ((modm, xK_o), runXPlus markContext config (withScreen W.view 1))
        , ((modm, xK_e), runXPlus markContext config (withScreen W.view 2))

        , ((modm .|. shiftMask, xK_a), runXPlus markContext config (withScreen W.shift 0))
        , ((modm .|. shiftMask, xK_o), runXPlus markContext config (withScreen W.shift 1))
        , ((modm .|. shiftMask, xK_e), runXPlus markContext config (withScreen W.shift 2))

        , ((modm .|. mod1Mask, xK_a), runXPlus markContext config (withScreen W.greedyView 0))
        , ((modm .|. mod1Mask, xK_o), runXPlus markContext config (withScreen W.greedyView 1))
        , ((modm .|. mod1Mask, xK_e), runXPlus markContext config (withScreen W.greedyView 2))

        -- Buttons programmed on my mouse.
        , ((shiftMask, xK_F1), click >> (withFocused $ windows . W.sink))
        , ((shiftMask, xK_F2), click >> sendMessage ToggleZoom)
        , ((shiftMask, xK_F3), click >> kill)
        ]

mapNumbersAndAlpha :: KeyMask -> (Char -> X ()) -> Map (KeyMask, KeySym) (X ())
mapNumbersAndAlpha km fn =
    mapNumbers km fn
      <> mapAlpha km fn

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
