
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.IO

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , terminal = "urxvt"
        , modMask = mod4Mask
        -- xmobar
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        -- workspace setup
        , workspaces = workspaces'
        } `additionalKeysP` keys'

-- Looks
-- workspaces
workspaces' :: [WorkspaceId]
workspaces' = ["1", "2", "3", "4", "5", "6",
               "7-writing", "8-music", "9-keys", "0-root" ]

-- Keybindings
keys' = [
    -- add keybindings here
    ] ++
    [ (shiftKey ++ "M-" ++ [key], action tag)
        | (tag, key) <- zip workspaces' "1234567890"
        , (shiftKey, action) <- [ ("", windows . W.view), ("S-", windows .W.shift) ]
    ]


