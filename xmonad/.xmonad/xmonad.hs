module Main where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import Graphics.X11.Xinerama (getScreenInfo)
import System.Exit
import System.IO
import System.Posix.Unistd
import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.SwapWorkspaces (swapWithCurrent)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.NoFrillsDecoration (noFrillsDeco)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Input
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce
import XMonad.Util.WindowProperties
import XMonad.Util.WorkspaceCompare

data Action
  = Increase
  | Decrease

myModMask :: KeyMask
myModMask = mod1Mask

myAppLauncherApp = "rofi -show run"

myBrowserApp = "google-chrome --no-sandbox"

myTerminalApp = "st"

myEditorApp = "emacsclient -c"

myEmacsDaemon = "emacs --daemon"

myIBusDaemon = "ibus-daemon"

myKeyboardSetting = "init-keyboard"

myBackgroundSetting = "bingwallpaper -1"

myCompositorApp = "compton"

myMailClient = "thunderbird"

volumeAction :: Action -> String
volumeAction Increase = "amixer --card 1 -q set PCM 5%+"
volumeAction Decrease = "amixer --card 1 -q set PCM 5%-"

volumeMasterAction :: Action -> String
volumeMasterAction Increase = "amixer --card 1 -q set Master 5%+"
volumeMasterAction Decrease = "amixer --card 1 -q set Master 5%-"

brightnessAction :: Action -> String
brightnessAction Increase = "xbacklight -steps 1 -time 1 -inc 8"
brightnessAction Decrease = "xbacklight -steps 1 -time 1 -dec 6"

commandPrompt :: XPConfig -> String -> M.Map String (X ()) -> X ()
commandPrompt c p m =
  inputPromptWithCompl c p (mkComplFunFromList (M.keys m)) ?+
  (\k -> fromMaybe (return ()) (M.lookup k m))

commands :: M.Map String (X ())
commands =
  M.fromList
    [ ("logout", io exitSuccess)
    , ("lock", spawn "xscreensaver-command -lock")
    , ( "suspend"
      , spawn
          "xscreensaver-command -lock && sleep 2 && sudo systemctl suspend -i")
    , ("shutdown", spawn "sleep 2 && systemctl poweroff")
    , ("restart", spawn "sleep 2 && systemctl reboot")
    , ( "sleep"
      , spawn "xscreensaver-command -lock && sleep 1 && sudo pm-suspend")
    ]

fireSPConfig :: XPConfig
fireSPConfig =
  def
    { font = "xft:Lucida Grande:bold:size=30"
    , bgColor = colorFocusBG
    , fgColor = colorNormalFG
    , bgHLight = colorNormalBG
    , fgHLight = colorFocusFG
    , borderColor = "black"
    , promptBorderWidth = 2
    , position = CenteredAt 0.5 0.5
    , height = 200
    , historySize = 256
    , defaultText = ""
    , autoComplete = Nothing
    }

myWorkspaces :: [String]
myWorkspaces = ["web", "code", "etc", "mail"]

myFullscreenHooks = [composeOne [isFullscreen -?> doFullFloat]]

myPlacement = withGaps (0, 0, 0, 0) (smart (0.5, 0.5))

myManagementHooks =
  composeAll . concat $
  [ [title =? t --> doFloat | t <- myOtherFloats]
  , [ fmap (c `L.isInfixOf`) className --> doShift (head myWorkspaces)
    | c <- myBrowsers
    ]
  , [className =? c --> doShift (myWorkspaces !! 3) | c <- ["Thunderbird"]]
  ]
  where
    myOtherFloats = ["urxvt", "rgt"]
    myBrowsers = ["Firefox"]

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.1 -- terminal height, 10%
    w = 1 -- terminal width, 100%
    t = 1 - h -- distance from top edge, 90%
    l = 1 - w -- distance from left edge, 0%

myNormalBorderColor = "#2e3440"

myFocusedBorderColor = "#2e3440"

colorNormalFG = "b6b4b8"

colorNormalBG = "#2f2e2b"

colorFocusFG = "#ffffff"

colorFocusBG = "#2f2e2b"

colorInActiveBG = "#2e3440"

colorInActiveTabBG = "#2e3440"

colorActiveBG = "#5e81ac"

colorVisibleWS = "#ebcb8b"

colorCurrentWS = "#bf616a"

colorTitle = "#a3be8c"

myTabConfig =
  def
    { inactiveBorderColor = colorInActiveTabBG
    , inactiveColor = colorInActiveTabBG
    , inactiveTextColor = colorInActiveBG
    , activeBorderColor = colorActiveBG
    , activeColor = colorActiveBG
    , activeTextColor = colorActiveBG
    , decoHeight = 10
    }

myTitleBarConfig =
  def
    { inactiveBorderColor = colorInActiveBG
    , inactiveColor = colorInActiveBG
    , inactiveTextColor = colorInActiveBG
    , activeBorderColor = colorActiveBG
    , activeColor = colorActiveBG
    , activeTextColor = colorActiveBG
    , decoHeight = 10
    }

delayedStartOnce :: Bool -> Int -> String -> X ()
delayedStartOnce greedyKill time run =
  let execName = takeWhile (/= ' ') run
      sleep = "sleep " ++ show time
      kills = "pkill " ++ execName
      howToRun = "bash -c \"" ++ run ++ "&\""
      ifkill = "if ! pgrep " ++ execName ++ "; then " ++ howToRun ++ "; fi;"
      ands = "; "
      wrap str = "bash -c '" ++ str ++ "'"
   in if greedyKill
        then spawn $
             wrap $ "(" ++ sleep ++ ands ++ kills ++ ands ++ howToRun ++ ") &"
        else spawn $ wrap $ "(" ++ sleep ++ ands ++ ifkill ++ ") &"

myStartup :: X ()
myStartup = do
  ewmhDesktopsStartup
  setDefaultCursor xC_left_ptr
  spawn
    "xrandr --output eDP-1 --mode 1368x768 --primary --auto --output HDMI-1 --mode 1920x1080 --right-of eDP-1 --auto"
  delayedStartOnce False 00 myCompositorApp
  delayedStartOnce False 00 myBackgroundSetting
  delayedStartOnce False 01 myTerminalApp
  delayedStartOnce False 01 myEmacsDaemon
  delayedStartOnce False 02 myMailClient
  spawn "xrdb ~/.Xresources &"
  spawn myIBusDaemon

spawnToWorkspace :: String -> String -> X ()
spawnToWorkspace program workspace = do
  spawnOnce program
  windows $ W.greedyView workspace

myAdditionalKeys :: [([Char], X ())]
myAdditionalKeys =
  [ ("M-C-j", windows W.swapUp)
  , ("M-C-k", windows W.swapDown)
  , ("M-S-c", kill)
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-i", sendMessage MirrorShrink)
  , ("M-u", sendMessage MirrorExpand)
  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-m", windows W.swapMaster)
  , ("M-t", withFocused $ windows . W.sink)
  , ("M-S-o", spawn myAppLauncherApp)
  , ("M-<Return>", spawn myTerminalApp)
  , ("M-S-<Return>", spawn myEditorApp)
  , ("M-S-b", spawn myBrowserApp)
  , ("M-C-9", spawn "xmonad --restart")
  , ("M-C-0", spawn "xmonad --recompile; xmonad --restart")
  ]

myComplexKeys :: [((KeyMask, KeySym), X ())]
myComplexKeys =
  [ ((mod1Mask, xK_F1), commandPrompt fireSPConfig "command" commands)
  , ((mod3Mask, xK_comma), sendMessage (IncMasterN 1))
  , ((mod3Mask, xK_period), sendMessage (IncMasterN (-1)))
  ]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
  [ ((m .|. modm, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_4]
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ] ++
  [ ( (m .|. modm .|. controlMask, key)
    , screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip [xK_q, xK_w, xK_e] [0 ..]
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ] ++
  [ ((modm .|. controlMask, k), windows $ swapWithCurrent i)
  | (i, k) <- zip myWorkspaces [xK_1 .. xK_4]
  ]

mouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    [ ( (modm, button1)
      , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((modm, button2), windows . (W.shiftMaster .) . W.focusWindow)
    ]

myLayout = deco $ stiled ||| Mirror stiled ||| tabbed shrinkText myTabConfig
  where
    stiled = spacingWithEdge 4 $ ResizableTall nmaster1 delta ratio _slaves
    nmaster1 = 1
    ratio = 1 / 2
    delta = 5 / 100
    _slaves = []
    deco = noFrillsDeco shrinkText myTitleBarConfig

myConfig =
  ewmh $
  def
    { manageHook =
        composeAll
          [ placeHook myPlacement
          , manageDocks
          , manageHook def
          , myManagementHooks
          , manageScratchPad
          , composeAll myFullscreenHooks
          ]
    , layoutHook = avoidStruts $ smartBorders myLayout
    , keys = myKeys
    , workspaces = myWorkspaces
    , startupHook = myStartup
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , modMask = myModMask
    , terminal = myTerminalApp
    , focusFollowsMouse = False
    } `additionalKeysP`
  myAdditionalKeys `additionalKeys`
  myComplexKeys

xineramaXmobarPP =
  xmobarPP
    { ppSort = getSortByXineramaPhysicalRule horizontalScreenOrderer
    , ppLayout = const ""
    , ppTitle = xmobarColor colorTitle ""
    , ppSep = "      |      "
    , ppWsSep = "  "
    , ppVisible = xmobarColor colorVisibleWS "" -- . shorten 100
    , ppCurrent = xmobarColor colorCurrentWS "" -- . shorten 100
    }

main = do
  n <- countScreens
  xmprocs <- mapM (\i -> spawnPipe $ "xmobar -x" ++ show i) [0 .. n - 1]
  let myLogHook =
        mapM_
          (\handle ->
             dynamicLogWithPP $ xineramaXmobarPP {ppOutput = hPutStrLn handle})
          xmprocs
  xmonad $ docks myConfig {logHook = myLogHook}
