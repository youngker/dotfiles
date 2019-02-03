module Keys where

import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Prompt.Shell
import XMonad.Hooks.Place
import Graphics.X11.ExtraTypes.XF86
import XMonad.Prompt.Window
import XMonad.Actions.WindowBringer
import XMonad.Layout.ResizableTile
import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL
import XMonad.Util.Scratchpad

import qualified Data.Map        as M
import qualified XMonad.StackSet as W
import XMonad.Actions.SwapWorkspaces (swapWithCurrent)

import Configs
import MyVars

myAdditionalKeys :: [([Char], X())]
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

  -- Launchers
  , ("M-S-p", spawn myAppLauncherApp)
  , ("M-<Return>", spawn myTerminalApp)
  , ("M-S-<Return>", spawn myEditorApp)
  , ("M-S-b", spawn myBrowserApp)

  -- System
  , ("M-S-q", spawn "xmonad --restart")
  , ("M-C-q", spawn "xmonad --recompile; xmonad --restart")
  ]

-- | Keys which don't exist in the simple default string mappings above
myComplexKeys :: [((KeyMask, KeySym), X())]
myComplexKeys =
  [ ((mod1Mask, xK_F12   ), commandPrompt fireSPConfig "command" commands)
  , ((mod3Mask, xK_comma ), sendMessage (IncMasterN 1)) -- Increment master win cnt
  , ((mod3Mask, xK_period), sendMessage (IncMasterN (-1))) -- Decrement master count
  -- , ((mod4Mask, xK_F1), spawn myBrowserApp)
  ]


-- | Key bindings. Add, modify or remove key bindings here.
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@ XConfig {XMonad.modMask = modm} = M.fromList $
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [
    ((m .|. modm, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_4]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]
  ++
  -- if you're on workspace 1, hitting mod-ctrl-5 will swap workspaces 1 and 5
  [
    ((modm .|. controlMask, k), windows $ swapWithCurrent i)
      | (i, k) <- zip myWorkspaces [xK_1 .. xK_4]
  ]


-- | Mouse bindings: default actions bound to mouse events
mouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings XConfig {XMonad.modMask = modm} = M.fromList
    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                          >> windows W.shiftMaster)
    -- mod-button2 %! Raise the window to the top of the stack
    , ((modm, button2), windows . (W.shiftMaster .) . W.focusWindow)
    ]
