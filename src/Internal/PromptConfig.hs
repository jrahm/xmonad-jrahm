module Internal.PromptConfig where

import XMonad.Prompt

xpConfig :: XPConfig
xpConfig = def {
               font = "xft:Source Code Pro:size=10"
             , bgColor = "#404040"
             , fgColor = "#8888ff"
             , promptBorderWidth = 0
             , height = 40
             }
