
-- Imports --
import System.IO

-- XMonad
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig

-- Layout
import XMonad.Layout.DwmStyle
import XMonad.Layout.SimpleDecoration

-- Actions
import XMonad.Actions.OnScreen
import XMonad.Actions.UpdatePointer

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks


-- Main --
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { manageHook = manageHook'
        , layoutHook = layoutHook'
        , terminal = "urxvt"
        , modMask = mod4Mask
        -- xmobar
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        } >> updatePointer (Relative 0.5 0.5)
        -- workspace setup
        , workspaces = workspaces'
        } `additionalKeysP` keys'

-- Hooks --
manageHook' :: ManageHook
manageHook' = manageDocks
                  <+> manageHook defaultConfig

layoutHook' = dwmStyle shrinkText sdTheme $ avoidStruts $ layoutHook defaultConfig

-- Looks --
-- workspaces
workspaces' :: [WorkspaceId]
workspaces' = ["1", "2", "3", "4", "5", "6",
               "7-writing", "8-music", "9-keys", "0-root" ]

chooseScreen :: WorkspaceId -> ScreenId
chooseScreen id = if length id > 1 then 0
                  else 1

-- decorations
sdTheme = defaultTheme
    { activeColor = "#284880"
    , activeTextColor = "#E0E0D0"
    , activeBorderColor = "#333344"
    , inactiveColor = "#222222"
    , inactiveTextColor = "#E0E0D0"
    , inactiveBorderColor = "#333300"
    }

-- Keybindings
-- keys
keys' = [
    -- add keybindings here
    ] ++
    [ (shiftKey ++ "M-" ++ [key], action tag)
        | (tag, key) <- zip workspaces' "1234567890"
        , (shiftKey, action) <- [ ("", windows . viewOnScreen (chooseScreen tag)), ("S-", windows . W.shift) ]
    ]


