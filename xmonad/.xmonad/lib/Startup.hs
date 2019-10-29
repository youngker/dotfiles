module Startup where

import           XMonad
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Util.Cursor
import           XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W

import MyVars

-- | Sleeps for provided time, then
--   Kills running instances of program (if greedyKill)
--   Then starts the program if greedyKill was enabled, or
--   if process wasn't running already
delayedStartOnce :: Bool -> Int -> String -> X ()
delayedStartOnce greedyKill time run =
  let execName = takeWhile (/= ' ') run
      sleep = "sleep " ++ show time
      kills = "pkill " ++ execName
      howToRun = "bash -c \"" ++ run ++ "&\""
      ifkill = "if ! pgrep " ++ execName ++ "; then " ++ howToRun ++ "; fi;"
      ands = "; "
      wrap str = "bash -c '" ++ str ++ "'"
  in if greedyKill then
      spawn $ wrap $ "(" ++ sleep ++ ands ++ kills ++ ands ++ howToRun ++ ") &"
    else
      spawn $ wrap $ "(" ++ sleep ++ ands ++ ifkill ++ ") &"

myStartup :: X ()
myStartup = do
  ewmhDesktopsStartup
  setDefaultCursor xC_left_ptr
  spawn "xrandr --output eDP-1 --mode 1368x768 --primary --auto --output HDMI-1 --mode 1920x1080 --right-of eDP-1 --auto"
  delayedStartOnce False 00 myCompositorApp
  delayedStartOnce False 00 myBackgroundSetting
  -- delayedStartOnce False 00 myKeyboardSetting
  delayedStartOnce False 01 myTerminalApp
  delayedStartOnce False 01 myEmacsDaemon
  delayedStartOnce False 02 myMailClient
  spawn "xrdb ~/.Xresources &"
  spawn "ibus-daemon"

spawnToWorkspace :: String -> String -> X ()
spawnToWorkspace program workspace = do
                                      spawnOnce program
                                      windows $ W.greedyView workspace
