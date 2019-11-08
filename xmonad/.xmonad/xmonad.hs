module Main where

import qualified Data.List                     as L
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Ratio
import           Graphics.X11.Xinerama          ( getScreenInfo )
import           System.Exit
import           System.IO
import           System.Posix.Unistd
import           XMonad
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.SwapWorkspaces  ( swapWithCurrent )
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeWindows
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Place
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Grid
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.NoBorders
import           XMonad.Layout.NoFrillsDecoration
                                                ( noFrillsDeco )
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Reflect
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.ShowWName
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.ManageHook
import           XMonad.Prompt
import           XMonad.Prompt.Input
import           XMonad.Prompt.RunOrRaise
import qualified XMonad.StackSet               as W
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.NamedWindows
import           XMonad.Util.Run                ( safeSpawn
                                                , spawnPipe
                                                )
import           XMonad.Util.Scratchpad
import           XMonad.Util.SpawnOnce
import           XMonad.Util.WindowProperties
import           XMonad.Util.WorkspaceCompare

myAppLauncherApp = "rofi -show run"

myBrowserApp = "google-chrome --no-sandbox"

myTerminalApp = "st"

myEditorApp = "emacsclient -c"

commandPrompt :: XPConfig -> String -> M.Map String (X ()) -> X ()
commandPrompt c p m =
    inputPromptWithCompl c p (mkComplFunFromList (M.keys m))
        ?+ (\k -> fromMaybe (return ()) (M.lookup k m))

commands :: M.Map String (X ())
commands = M.fromList
    [ ("logout", io exitSuccess)
    , ("lock"  , spawn "xscreensaver-command -lock")
    , ( "suspend"
      , spawn
          "xscreensaver-command -lock && sleep 2 && sudo systemctl suspend -i"
      )
    , ("shutdown", spawn "sleep 2 && systemctl poweroff")
    , ("restart" , spawn "sleep 2 && systemctl reboot")
    , ( "sleep"
      , spawn "xscreensaver-command -lock && sleep 1 && sudo pm-suspend"
      )
    ]

_XPConfig :: XPConfig
_XPConfig = def { font              = "xft:Lucida Grande:bold:size=15"
                , promptBorderWidth = 0
                , position          = CenteredAt 0.1 0.7
                , height            = 50
                , historySize       = 256
                , bgColor           = "black"
                , fgColor           = "white"
                , bgHLight          = color CurrentWorkspace
                , defaultText       = ""
                , autoComplete      = Nothing
                }

myFullscreenHooks = [composeOne [isFullscreen -?> doFullFloat]]

myPlacement = withGaps (0, 0, 0, 0) (smart (0.5, 0.5))

myManagementHooks =
    composeAll
        . concat
        $ [ [className =? "Mail" --> doShift "mail"]
          , [className =? "Google-chrome" --> doShift "web"]
          , [className =? "Firefox" --> doShift "web"]
          , [className =? "Emacs" --> doShift "code"]
          , [(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]
          , [ className =? c --> doFloat | c <- myFloats ]
          ]
    where myFloats = ["rgt", "Org.gnome.Nautilus", "Gnome-terminal"]

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.1 -- terminal height, 10%
    w = 1 -- terminal width, 100%
    t = 1 - h -- distance from top edge, 90%
    l = 1 - w -- distance from left edge, 0%

data Colors
  = NormalBorder
  | FocusedBorder
  | InActiveBackground
  | InActiveTabBackground
  | ActiveBackground
  | VisibleWorkspace
  | CurrentWorkspace
  | CurrentTitle
  deriving Show

color :: Colors -> String
color x = case x of
    NormalBorder          -> "#2e3440"
    FocusedBorder         -> "#2e3440"
    InActiveBackground    -> "#2e3440"
    InActiveTabBackground -> "#2e3440"
    ActiveBackground      -> "#5e81ac"
    VisibleWorkspace      -> "#ebcb8b"
    CurrentWorkspace      -> "#bf616a"
    CurrentTitle          -> "#a3be8c"

myTabConfig = def { inactiveBorderColor = color InActiveTabBackground
                  , inactiveColor       = color InActiveTabBackground
                  , inactiveTextColor   = color InActiveBackground
                  , activeBorderColor   = color ActiveBackground
                  , activeColor         = color ActiveBackground
                  , activeTextColor     = color ActiveBackground
                  , decoHeight          = 10
                  }

myTitleBarConfig = def { inactiveBorderColor = color InActiveBackground
                       , inactiveColor       = color InActiveBackground
                       , inactiveTextColor   = color InActiveBackground
                       , activeBorderColor   = color ActiveBackground
                       , activeColor         = color ActiveBackground
                       , activeTextColor     = color ActiveBackground
                       , decoHeight          = 10
                       }

confShowWName = def { swn_font    = "xft:Lucida Grande:bold:size=50"
                    , swn_bgcolor = color InActiveBackground
                    , swn_color   = color ActiveBackground
                    , swn_fade    = 2
                    }

startup =
    [ "xrandr --output eDP-1 --mode 1368x768 --primary --auto \
    \--output HDMI-1 --mode 1920x1080 --right-of eDP-1 --auto"
    , "compton"
    , "bingwallpaper -1"
    , "emacs --daemon"
    , "xrdb ~/.Xresources"
    , "ibus-daemon"
    , "setxkbmap -option ctrl:nocaps"
    ]

data Workspace =
  WS
    { workspaceName :: String
    , workspaceAction :: X ()
    }

workspace =
    [ WS "web" $ spawn "google-chrome"
    , WS "code" $ spawn "st"
    , WS "etc" $ spawn "st"
    , WS "mail" $ spawn "thunderbird"
    , WS "vm" $ spawn "st"
    ]

additionalKey :: [(String, X ())]
additionalKey =
    [ ("M-S-c"       , kill)
    , ("M-<Space>"   , sendMessage NextLayout)
    , ("M-<Tab>"     , windows W.focusDown)
    , ("M-j"         , windows W.focusDown)
    , ("M-S-j"       , windows W.swapDown)
    , ("M-k"         , windows W.focusUp)
    , ("M-S-k"       , windows W.swapUp)
    , ("M-m"         , windows W.swapMaster)
    , ("M-u"         , sendMessage (IncMasterN 1))
    , ("M-i"         , sendMessage (IncMasterN (-1)))
    , ("M-h"         , sendMessage Shrink)
    , ("M-l"         , sendMessage Expand)
    , ("M-z"         , sendMessage $ Toggle FULL)
    , ("M-t"         , withFocused $ windows . W.sink)
    , ("M-S-o"       , spawn myAppLauncherApp)
    , ("M-<Return>"  , spawn myTerminalApp)
    , ("M-S-<Return>", spawn myEditorApp)
    , ("M-S-b"       , spawn myBrowserApp)
    , ("M-C-9"       , spawn "xmonad --restart")
    , ("M-C-0", spawn "xmonad --recompile; xmonad --restart")
    ]

--  , ("M-S-<Space>", setLayout $ XMonad.layoutHook conf)
myComplexKeys :: [((KeyMask, KeySym), X ())]
myComplexKeys =
    [ ((mod1Mask, xK_F1)    , commandPrompt _XPConfig "command" commands)
    , ((mod1Mask, xK_F2)    , runOrRaisePrompt _XPConfig)
    , ((mod3Mask, xK_comma) , sendMessage (IncMasterN 1))
    , ((mod3Mask, xK_period), sendMessage (IncMasterN (-1)))
    ]

keyboard conf@XConfig { XMonad.modMask = modm } =
    M.fromList
        $  [ ((m .|. modm, k), windows $ f i)
           | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
           , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
           ]
        ++ [ ( (m .|. modm, key)
             , screenWorkspace sc >>= flip whenJust (windows . f)
             )
           | (key, sc) <- zip [xK_bracketleft, xK_bracketright] [0 ..]
           , (f  , m ) <- [(W.view, 0), (W.shift, shiftMask)]
           ]

layout = deco $ stiled ||| Mirror stiled ||| tabbed shrinkText myTabConfig
  where
    stiled   = spacingWithEdge 4 $ ResizableTall nmaster1 delta ratio _slaves
    nmaster1 = 1
    ratio    = 1 / 2
    delta    = 5 / 100
    _slaves  = []
    deco     = noFrillsDeco shrinkText myTitleBarConfig

data AllFloats = AllFloats deriving (Read, Show)

instance SetsAmbiguous AllFloats where
    hiddens _ wset _ _ _ = M.keys $ W.floating wset

configuration =
    ewmh
        $                 def
                              { manageHook         = composeAll
                                                         [ placeHook myPlacement
                                                         , manageDocks
                                                         , manageHook def
                                                         , myManagementHooks
                                                         , manageScratchPad
                                                         , composeAll myFullscreenHooks
                                                         ]
                              , layoutHook         = showWName' confShowWName
                                                     $ lessBorders AllFloats
                                                     $ mkToggle (single FULL)
                                                     $ avoidStruts
                                                     $ smartBorders layout
                              , workspaces         = map workspaceName workspace
                              , startupHook        = mapM_ spawn startup
                              , keys               = keyboard
                              , normalBorderColor  = color NormalBorder
                              , focusedBorderColor = color FocusedBorder
                              , modMask            = mod1Mask
                              , terminal           = myTerminalApp
                              , focusFollowsMouse  = False
                              }
        `additionalKeysP` additionalKey
        `additionalKeys`  myComplexKeys

_xmobarPP = xmobarPP
    { ppSort    = getSortByXineramaPhysicalRule horizontalScreenOrderer
    , ppLayout  = const ""
    , ppTitle   = xmobarColor (color CurrentTitle) ""
    , ppSep     = "      |      "
    , ppWsSep   = "  "
    , ppVisible = xmobarColor (color VisibleWorkspace) ""
    , ppCurrent = xmobarColor (color CurrentWorkspace) ""
    }

fadeLogHook = fadeWindowsLogHook fadeHook
  where
    fadeHook :: FadeHook
    fadeHook = composeAll
        [opaque, isUnfocused --> transparency 0.05, isFloating --> opaque]

main = do
    nScreens <- countScreens
    xmprocs  <- mapM (\i -> spawnPipe $ "xmobar -x" ++ show i)
                     [0 .. nScreens - 1]
    let
        loghook = mapM_
            (\handle ->
                dynamicLogWithPP $ _xmobarPP { ppOutput = hPutStrLn handle }
            )
            xmprocs
    xmonad $ docks configuration { logHook = composeAll [loghook, fadeLogHook] }
