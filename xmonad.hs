
-- Imports --
import System.IO
import System.Process

-- XMonad
import XMonad
import XMonad.Core
import qualified XMonad.StackSet as W
import XMonad.Util.Run(spawnPipe, runProcessWithInput)
import XMonad.Util.EZConfig

-- Layout
import XMonad.Layout.Groups.Wmii
import XMonad.Layout.SimpleDecoration

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.OnScreen
import XMonad.Actions.UpdatePointer

-- Hooks
import XMonad.Hooks.DebugStack
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

-- Main --
main = do
    xmproc1 <- spawnPipe "xmobar -x 0"
    xmproc2 <- spawnPipe "xmobar -x 1"
    xmonad $ defaultConfig
        { manageHook = manageHook'
        , layoutHook = layoutHook'
        , terminal = terminal_cmd
        , modMask = mod4Mask
        -- xmobar
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = \a -> do hPutStrLn xmproc1 a
                                              hPutStrLn xmproc2 a
                        , ppLayout = \s -> []
                        , ppHidden = xmobarColor "#D0D0D0" "" . id
                        , ppHiddenNoWindows = xmobarColor "#808080" "" . id
                        , ppTitle = xmobarColor "green" "" . shorten 120
                        } >> (local (\c -> c { mouseFocused = False }) $ updatePointer (Relative 0.5 0.5))
        -- workspace setup
        , workspaces = workspaces'
        , focusFollowsMouse = True
        } `additionalKeysP` keys'

-- Hooks --
manageHook' :: ManageHook
manageHook' = manageDocks
                  <+> manageHook defaultConfig

layoutHook' = avoidStruts $ wmii shrinkText sdTheme

-- Setup --
terminal_cmd :: String
terminal_cmd = "urxvt"

secondDisplay :: X ScreenId
secondDisplay = io $ do
    s <- runProcessWithInput "/home/apoelstra/.xmonad/monitor-count.sh" [] ""
    return (((S. read) s) - 1)

-- Looks --
-- workspaces
workspaces' :: [WorkspaceId]
workspaces' = ["1", "2", "3", "4", "5-mail", "6-irc",
               "7-writing", "8-music", "9-keys", "root" ]

chooseScreen :: WorkspaceId -> X ScreenId
chooseScreen id = if id !! 0 > '6' then do return 0
                   else secondDisplay

viewOnSecond :: WorkspaceId -> X ()
viewOnSecond = \tag -> (chooseScreen tag)
                   >>= (return . viewOnScreen)
                   >>= (\fn -> windows (fn tag))

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
keys' = [ -- wmii keybindings
          -- group modes
          ("M-m", groupToFullLayout)
        , ("M-s", groupToTabbedLayout)
        , ("M-d", groupToVerticalLayout)
        , ("M-f", toggleGroupFull)
          -- window resizing
        , ("M-[", zoomGroupOut)
        , ("M-]", zoomGroupIn)
          -- window movement
        , ("M-<Space>", toggleFocusFloat)
        , ("M-j", focusDown)
        , ("M-k", focusUp)
        , ("S-M-j", swapDown)
        , ("S-M-k", swapUp)
        , ("M-h", focusGroupUp)
        , ("M-l", focusGroupDown)
        , ("S-M-h", moveToGroupUp False)
        , ("S-M-l", moveToGroupDown False)
        -- misc
        , ("S-M-<Delete>", spawn "touch /tmp/.cancel-shutdown")
        , ("M-<Tab>", nextScreen)
        , ("M-<Return>", spawn $ terminal_cmd)
        , ("S-M-c", kill)
    -- add keybindings here
    ] ++
    [ (shiftKey ++ "M-" ++ [key], action tag)
        | (tag, key) <- zip workspaces' "1234567890"
        , (shiftKey, action) <- [
            ("", viewOnSecond),
            ("S-", windows . W.shift)
        ]
    ]


