module MyVars where

import XMonad

data Action = Increase | Decrease

myModMask :: KeyMask
myModMask = mod3Mask

myAppLauncherApp :: String
myAppLauncherApp = "rofi -show run"

myBrowserApp :: String
myBrowserApp = "google-chrome --no-sandbox"

myTerminalApp :: String
myTerminalApp = "st"

myEditorApp :: String
myEditorApp = "emacs"

myEmacsDaemon :: String
myEmacsDaemon = "emacs --daemon"

myKeyboardSetting :: String
myKeyboardSetting = "init-keyboard"

myBackgroundSetting :: String
myBackgroundSetting = "bingwallpaper -1"

myCompositorApp :: String
myCompositorApp = "compton"

myMailClient :: String
myMailClient = "thunderbird-bin"

volumeAction :: Action -> String
volumeAction Increase = "amixer --card 1 -q set PCM 5%+"
volumeAction Decrease = "amixer --card 1 -q set PCM 5%-"

volumeMasterAction :: Action -> String
volumeMasterAction Increase = "amixer --card 1 -q set Master 5%+"
volumeMasterAction Decrease = "amixer --card 1 -q set Master 5%-"

brightnessAction :: Action -> String
brightnessAction Increase = "xbacklight -steps 1 -time 1 -inc 8"
brightnessAction Decrease = "xbacklight -steps 1 -time 1 -dec 6"
