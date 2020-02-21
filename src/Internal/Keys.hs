module Internal.Keys where

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

type KeyMap l = XConfig l -> Map (KeyMask, KeySym) (X ())

applyKeys :: XConfig l -> IO (XConfig l)
applyKeys config@(XConfig {modMask = modm}) = do
   ks <- newKeys
   withWindowNavigation (xK_k, xK_h, xK_j, xK_l) $
     config { keys = ks }

data WinPrompt = WinPrompt

instance XPrompt WinPrompt where
    showXPrompt _ = "[Window] "
    commandToComplete _ = id

getHorizontallyOrderedScreens ::
  W.StackSet wid l a ScreenId ScreenDetail ->
    [W.Screen wid l a ScreenId ScreenDetail]
-- ^ Returns a list of screens ordered from leftmost to rightmost.
getHorizontallyOrderedScreens windowSet =
    flip sortBy screens $ \sc1 sc2 ->
      let (SD (Rectangle x1 _ _ _)) = W.screenDetail sc1
          (SD (Rectangle x2 _ _ _)) = W.screenDetail sc2
          in x1 `compare` x2
    where
      screens = W.current windowSet : W.visible windowSet

newKeys :: IO (KeyMap l)
newKeys =
  withNewMarkContext $ \markContext ->
    return $ \config@(XConfig {modMask = modm}) ->
      let workspacesByInt =
            Map.fromList $
              zip ['1'..] (XMonad.workspaces config)

          gotoWorkspace ch = do
            saveLastMark markContext
            windows $ W.greedyView $ return ch

          shiftToWorkspace ch = do
            windows $ W.shift $ return ch

          fuzzyCompletion s1 s0 =
            let ws = filter (not . all isSpace) $ words (map toLower s1)
                l0 = map toLower s0 in
                 all (`isInfixOf`l0) ws

          getString = runQuery $ do
              t <- title
              a <- appName
              return $
                if map toLower a `isInfixOf` map toLower t
                  then t
                  else printf "%s - %s" a t

          withScreen :: (WorkspaceId -> WindowSet -> WindowSet) -> Int -> X ()
          withScreen fn n = do
            saveLastMark markContext
            windows $ \windowSet ->
              case (getHorizontallyOrderedScreens windowSet !! n) of
                Nothing -> windowSet
                Just screen -> fn (W.tag $ W.workspace screen) windowSet

          windowJump = do
              windowTitlesToWinId <- withWindowSet $ \ss ->
                Map.fromList <$>
                  mapM (\wid -> (,) <$> getString wid <*> return wid)
                       (W.allWindows ss)

              mkXPrompt
                WinPrompt
                xpConfig
                (\input -> do
                  return $ filter (fuzzyCompletion input) (Map.keys windowTitlesToWinId)) $ \str -> do
                    saveLastMark markContext
                    case Map.lookup str windowTitlesToWinId of
                        Just w -> focus w
                        Nothing ->
                          case filter (fuzzyCompletion str) (Map.keys windowTitlesToWinId) of
                              [s] ->
                                mapM_ focus (Map.lookup s windowTitlesToWinId)
                              _ -> return ()

          in

      Map.fromList
        [ ((modm, xK_F12), (void $ spawn "spotify-control next"))
        , ((modm, xK_F11), (void $ spawn "spotify-control prev"))
        , ((modm, xK_semicolon), scratchpadSpawnActionTerminal "scratchpad")
        , ((modm, xK_F10), (void $ spawn "spotify-control play"))
        , ((modm, xK_r),   (void $ spawn "dmenu_run"))
        , ((modm .|. shiftMask, xK_r),   (void $ spawn "gmrun"))
        , ((modm .|. mod1Mask, xK_l), (void $ spawn "xsecurelock"))
        , ((modm .|. mod1Mask, xK_s), (void $ spawn "xsecurelock && pm-suspend"))
        , ((modm .|. shiftMask, xK_c), kill)
        , ((modm .|. shiftMask, xK_t), withFocused $ windows . W.sink)
        , ((modm, xK_t),  (void $ spawn (terminal config)))
        , ((modm, xK_m), (submap $ mapAlpha modm (markCurrentWindow markContext)))
        , ((modm, xK_w), windowJump)
        , ((modm, xK_apostrophe), (submap $
              Map.insert
                (modm, xK_apostrophe)
                (jumpToLast markContext)
                (mapAlpha modm (jumpToMark markContext))))
        , ((modm, xK_g), (submap $ mapNumbersAndAlpha 0 gotoWorkspace))
        , ((modm .|. shiftMask, xK_g), (submap $ mapNumbersAndAlpha 0 shiftToWorkspace))

        , ((modm .|. shiftMask, xK_bracketleft), sendMessage (IncMasterN (-1)))
        , ((modm .|. shiftMask, xK_bracketright), sendMessage (IncMasterN 1))
        , ((modm, xK_bracketleft), sendMessage ShrinkZoom)
        , ((modm, xK_bracketright), sendMessage ExpandZoom)

        , ((modm, xK_space), sendMessage NextLayout)

        , ((modm, xK_q), spawn "xmonad --recompile && xmonad --restart")
        , ((modm, xK_z), sendMessage ToggleZoom)

        , ((modm, xK_Tab), windows W.focusDown)
        , ((modm .|. shiftMask, xK_Tab), windows W.focusUp)

        , ((modm, xK_a), withScreen W.view 0)
        , ((modm, xK_o), withScreen W.view 1)
        , ((modm, xK_e), withScreen W.view 2)

        , ((modm .|. shiftMask, xK_a), withScreen W.shift 0)
        , ((modm .|. shiftMask, xK_o), withScreen W.shift 1)
        , ((modm .|. shiftMask, xK_e), withScreen W.shift 2)
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
