
-- Imports --
import System.IO
import System.Process
import System.Posix.IO

-- XMonad
import XMonad
import XMonad.Core
import qualified XMonad.StackSet as W
import XMonad.Util.Run(spawnPipe, runProcessWithInput)
import XMonad.Util.EZConfig

-- Layout
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Groups.Wmii
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.OnScreen
import XMonad.Actions.UpdatePointer

-- Hooks
import XMonad.Hooks.DebugStack
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

-- Main --
main = do
    -- hack from geekosaur to get stderr to a readable place
    -- closeFd 2 >> openFd ".xsession-errors" WriteOnly (Just 0644) defaultFileFlags
    -- regular code
    xmproc1 <- spawnPipe "xmobar -x 0"
    xmproc2 <- spawnPipe "xmobar -x 1"
    xmproc3 <- spawnPipe "xmobar -x 2"
    xmonad $ def
        { manageHook = manageHook'
        , layoutHook = layoutHook'
        , terminal = terminal_cmd
        , modMask = mod4Mask
        -- xmobar
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = \a -> do hPutStrLn xmproc1 a
                                              hPutStrLn xmproc2 a
                                              hPutStrLn xmproc3 a
                        , ppLayout = \s -> []
                        , ppHidden = xmobarColor "#D0D0D0" "" . id
                        , ppHiddenNoWindows = xmobarColor "#808080" "" . id
                        , ppTitle = xmobarColor "green" "" . shorten 120
                        } >> (local (\c -> c { mouseFocused = False }) $ updatePointer (0.5, 0.5) (0, 0))
        -- workspace setup
        , workspaces = workspaces'
        , focusFollowsMouse = False -- interacts poorly with wmii
        , handleEventHook = docksEventHook
        -- extra
        , borderWidth = 5
        } `additionalKeysP` keys'

-- Hooks --
manageHook' :: ManageHook
manageHook' = manageDocks
                  <+> (isFullscreen --> doFullFloat)
                  <+> manageHook def

layoutHook' = avoidStruts $ wmii shrinkText sdTheme

-- Setup --
terminal_cmd :: String
terminal_cmd = "urxvt -pe selection-to-clipboard"

secondDisplay :: X ScreenId
secondDisplay_ :: X ScreenId
secondDisplay_ = io $ do
    s <- runProcessWithInput "/home/apoelstra/.xmonad/monitor-count.sh" [] ""
    let n = read s
    let n_adjust = n - 1
    return (S n_adjust)

thirdDisplay :: X ScreenId
thirdDisplay_ :: X ScreenId
thirdDisplay_ = io $ do
    s <- runProcessWithInput "/home/apoelstra/.xmonad/monitor-count.sh" [] ""
    let n = read s
    let n_adjust = if n == 1 then 0
                   else 1
    return (S n_adjust)

-- overrides for Portland
secondDisplay = io $ return 1
thirdDisplay = io $ return 0

-- Looks --
-- workspaces
workspaces' :: [WorkspaceId]
workspaces' = ["1", "2", "3", "4", "5",
               "Q", "W", "E", "R", "T",
               "6", "7", "8", "9", "0",
               "Y", "U", "I", "O", "P",
               "-1", "-2", "-3", "-4" ]

chooseScreen :: WorkspaceId -> X ScreenId
chooseScreen id | id == "1" = secondDisplay
                | id == "2" = secondDisplay
                | id == "3" = secondDisplay
                | id == "4" = secondDisplay
                | id == "5" = secondDisplay
                | id == "Q" = secondDisplay
                | id == "W" = secondDisplay
                | id == "E" = secondDisplay
                | id == "R" = secondDisplay
                | id == "T" = secondDisplay
                | id == "6" = thirdDisplay
                | id == "7" = thirdDisplay
                | id == "8" = thirdDisplay
                | id == "9" = thirdDisplay
                | id == "0" = thirdDisplay
                | id == "Y" = thirdDisplay
                | id == "U" = thirdDisplay
                | id == "I" = thirdDisplay
                | id == "O" = thirdDisplay
                | id == "P" = thirdDisplay
                | id == "-1" = return 0
                | id == "-2" = return 0
                | id == "-3" = return 0
                | id == "-4" = return 0

viewOnSecond :: WorkspaceId -> X ()
viewOnSecond = \tag -> (chooseScreen tag)
                   >>= (return . viewOnScreen)
                   >>= (\fn -> windows (fn tag))

-- decorations
sdTheme = def
    { activeColor = "#284880"
    , activeTextColor = "#E0E0D0"
    , activeBorderColor = "#333344"
    , inactiveColor = "#222222"
    , inactiveTextColor = "#E0E0D0"
    , inactiveBorderColor = "#333300"
    , fontName = "xft:xos4 Terminus:style=Regular:size=11"
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
        , ("M-<Down>", focusDown)
        , ("M-<Up>", focusUp)
        , ("S-M-<Down>", swapDown)
        , ("S-M-<Up>", swapUp)
        , ("M-<Left>", focusGroupUp)
        , ("M-<Right>", focusGroupDown)
        , ("S-M-<Left>", moveToGroupUp False)
        , ("S-M-<Right>", moveToGroupDown False)
        -- override numbers since default behaviour (switch to workspace on laptop screen) is surprising
        , ("M-1", spawn $ "echo")
        , ("M-2", spawn $ "echo")
        , ("M-3", spawn $ "echo")
        , ("M-4", spawn $ "echo")
        , ("M-5", spawn $ "echo")
        , ("M-6", spawn $ "echo")
        , ("M-7", spawn $ "echo")
        , ("M-8", spawn $ "echo")
        , ("M-9", spawn $ "echo")
        , ("M-0", spawn $ "echo")
        -- fn keys
        , ("<XF86MonBrightnessUp>", spawn "brightnessctl s +1%")
        , ("<XF86MonBrightnessDown>", spawn "brightnessctl s 1%-")
        , ("S-<XF86MonBrightnessUp>", spawn "brightnessctl s +10%")
        , ("S-<XF86MonBrightnessDown>", spawn "brightnessctl s 10%-")
        , ("<XF86Favorites>", spawn "/home/apoelstra/bin/seizure.sh")
        -- misc
        , ("M-<Tab>", nextScreen)
        , ("M-`", spawn "idevicediagnostics restart")
        , ("S-M-<Delete>", spawn "touch /tmp/.cancel-shutdown")
        , ("S-M-<Return>", spawn $ terminal_cmd ++ " -fn " ++ "-*-terminus-medium-*-*-*-14-*-*-*-*-*-*-*")
        , ("M-<Return>", spawn $ terminal_cmd)
        , ("M-<Backspace>", spawn $ terminal_cmd ++ " -e bash -c 'source ~/.bashrc && ssh camus'")
        , ("S-M-c", kill)
        , ("M-q", spawn $ "xmonad --recompile && xmonad --restart")
    -- add keybindings here
    ] ++
    [ (shiftKey ++ "M-" ++ key, action tag)
        | (tag, key) <- zip workspaces' ["<F1>", "<F2>", "<F3>", "<F4>", "<F5>",
                                         "<F11>", "<F12>", "<F13>", "<F14>", "<F15>",
                                         "<F6>", "<F7>", "<F8>", "<F9>", "<F10>",
                                         "<F16>", "<F17>", "<F18>", "<F19>", "<F20>",
                                         "<F21>", "<F22>", "<F23>", "<F24>"
                                         ]
        , (shiftKey, action) <- [
            ("", viewOnSecond),
            ("S-", windows . W.shift)
        ]
    ]


