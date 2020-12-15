module Main where

import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.RunOrRaise
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare

myAppLauncherApp = "rofi -show run"

myBrowserApp = "google-chrome --no-sandbox"

myTerminalApp = "st"

myEditorApp = "emacsclient -c"

commandPrompt :: XPConfig -> String -> M.Map String (X ()) -> X ()
commandPrompt c p m =
  inputPromptWithCompl c p (mkComplFunFromList (M.keys m))
    ?+ (\k -> fromMaybe (return ()) (M.lookup k m))

commands :: M.Map String (X ())
commands =
  M.fromList
    [ ("logout", io exitSuccess),
      ("lock", spawn "xscreensaver-command -lock"),
      ("suspend", spawn "xscreensaver-command -lock && sleep 2 && sudo systemctl suspend -i"),
      ("shutdown", spawn "sleep 2 && systemctl poweroff"),
      ("restart", spawn "sleep 2 && systemctl reboot"),
      ("sleep", spawn "xscreensaver-command -lock && sleep 1 && sudo pm-suspend")
    ]

_XPConfig :: XPConfig
_XPConfig =
  def
    { font = "xft:Lucida Grande:bold:size=20",
      promptBorderWidth = 0,
      position = CenteredAt 0.1 0.7,
      height = 50,
      historySize = 256,
      bgColor = color InActiveTabBackground,
      fgColor = color CurrentTitle,
      bgHLight = color VisibleWorkspace,
      defaultText = "",
      autoComplete = Nothing
    }

myPlacement = withGaps (0, 0, 0, 0) (smart (0.5, 0.5))

myManagementHooks =
  composeAll
    [ className =? "Mail" --> doShift "mail",
      className =? "Google-chrome" --> doShift "web",
      className =? "Firefox" --> doShift "web",
      className =? "clipboard-google-translate" --> doFloat
    ]

data Colors
  = NormalBorder
  | FocusedBorder
  | InActiveBackground
  | InActiveTabBackground
  | ActiveBackground
  | VisibleWorkspace
  | CurrentWorkspace
  | CurrentTitle
  deriving (Show)

color :: Colors -> String
color x = case x of
  NormalBorder -> "#2e3440"
  FocusedBorder -> "#2e3440"
  InActiveBackground -> "#2e3440"
  InActiveTabBackground -> "#2e3440"
  ActiveBackground -> "#5e81ac"
  VisibleWorkspace -> "#ebcb8b"
  CurrentWorkspace -> "#bf616a"
  CurrentTitle -> "#a3be8c"

myTabConfig =
  def
    { inactiveBorderColor = color InActiveTabBackground,
      inactiveColor = color InActiveTabBackground,
      inactiveTextColor = color InActiveBackground,
      activeBorderColor = color ActiveBackground,
      activeColor = color ActiveBackground,
      activeTextColor = color ActiveBackground,
      decoHeight = 10
    }

myTitleBarConfig =
  def
    { inactiveBorderColor = color InActiveBackground,
      inactiveColor = color InActiveBackground,
      inactiveTextColor = color InActiveBackground,
      activeBorderColor = color ActiveBackground,
      activeColor = color ActiveBackground,
      activeTextColor = color ActiveBackground,
      decoHeight = 10
    }

confShowWName =
  def
    { swn_font = "xft:Lucida Grande:bold:size=100",
      swn_bgcolor = color InActiveBackground,
      swn_color = color ActiveBackground,
      swn_fade = 2
    }

startup =
  [ "xrandr --output HDMI-0 --mode 3840x2160 --primary --scale 0.65x0.65",
    "compton",
    "bingwallpaper -1",
    "emacs --daemon",
    "ibus-daemon",
    "xrdb ~/.Xresources",
    "setxkbmap -option ctrl:nocaps"
  ]

data Workspace = WS
  { workspaceName :: String,
    workspaceAction :: X ()
  }

workspace =
  [ WS "code" $ spawn "st",
    WS "web" $ spawn "google-chrome",
    WS "etc" $ spawn "st",
    WS "mail" $ spawn "thunderbird",
    WS "vm" $ spawn "st"
  ]

additionalKey :: [(String, X ())]
additionalKey =
  [ ("M-<Delete>", kill),
    ("M-<Space>", sendMessage NextLayout),
    ("M-<Tab>", windows W.focusDown),
    ("M-j", windows W.focusDown),
    ("M-S-j", windows W.swapDown),
    ("M-k", windows W.focusUp),
    ("M-S-k", windows W.swapUp),
    ("M-m", windows W.swapMaster),
    ("M-u", sendMessage (IncMasterN 1)),
    ("M-i", sendMessage (IncMasterN (-1))),
    ("M-h", sendMessage Shrink),
    ("M-l", sendMessage Expand),
    ("M-z", sendMessage $ Toggle FULL),
    ("M-t", withFocused $ windows . W.sink),
    ("M-<Return>", spawn myAppLauncherApp),
    ("M-S-<Return>", spawn myEditorApp),
    ("M-S-b", spawn myBrowserApp),
    ("<F9>", spawn "xmonad --recompile"),
    ("<F10>", spawn "xmonad --recompile; xmonad --restart")
  ]

myComplexKeys :: [((KeyMask, KeySym), X ())]
myComplexKeys =
  [ ((mod1Mask, xK_F1), commandPrompt _XPConfig "command" commands),
    ((mod1Mask, xK_F2), runOrRaisePrompt _XPConfig)
  ]

keyboard conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
    [ ((m .|. modm, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
        (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
      ++ [ ( (m .|. modm, key),
             screenWorkspace sc >>= flip whenJust (windows . f)
           )
           | (key, sc) <- zip [xK_bracketleft, xK_bracketright] [0 ..],
             (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
         ]

layout = deco $ tall ||| Mirror tall ||| grid ||| tab
  where
    tall = spacingWithEdge 4 $ ResizableTall 1 (1 / 10) (1 / 2) []
    grid = spacingWithEdge 4 Grid
    deco = noFrillsDeco shrinkText myTitleBarConfig
    tab = tabbed shrinkText myTabConfig

fadeLogHook = fadeWindowsLogHook fadeHook
  where
    fadeHook :: FadeHook
    fadeHook = opaque <+> (isUnfocused --> transparency 0.1) <+> (isFloating --> opaque)

myXmobar :: (Int, Handle) -> X ()
myXmobar (screenId, xmobarPipe) = do
  dynamicLogWithPP $
    xmobarPP
      { ppOutput = hPutStrLn xmobarPipe,
        ppLayout = const "",
        ppTitle = xmobarColor (color CurrentTitle) "",
        ppSep = "      |      ",
        ppWsSep = "  ",
        ppVisible = xmobarColor (color VisibleWorkspace) "",
        ppCurrent = xmobarColor (color CurrentWorkspace) ""
      }

xmobarCmd :: MonadIO m => Int -> Int -> m (Int, Handle)
xmobarCmd nScreens screen = do
  xmobarPipe <- spawnPipe cmd
  pure (screen, xmobarPipe)
  where
    cmd = "xmobar ~/.xmonad/xmobar.hs --screen=" <> show (succ nScreens - screen)

main :: IO ()
main = do
  nScreens <- countScreens
  xmobarPipes <- traverse (xmobarCmd nScreens) [1 .. nScreens]
  xmonad $
    docks
      def
        { terminal = myTerminalApp,
          manageHook = manageDocks <+> manageHook def <+> placeHook myPlacement <+> myManagementHooks,
          layoutHook = showWName' confShowWName $ mkToggle (single FULL) $ avoidStruts $ smartBorders layout,
          workspaces = map workspaceName workspace,
          startupHook = traverse_ spawn startup,
          logHook = fadeLogHook <+> traverse_ myXmobar xmobarPipes,
          keys = keyboard,
          normalBorderColor = color NormalBorder,
          focusedBorderColor = color FocusedBorder,
          modMask = mod1Mask,
          focusFollowsMouse = False
        }
      `additionalKeysP` additionalKey
      `additionalKeys` myComplexKeys
