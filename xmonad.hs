
-- Imports --
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86 -- media keys
import System.IO
import System.Process
import System.Posix.IO

-- XMonad
import XMonad
import XMonad.Core
import qualified XMonad.StackSet as W
import XMonad.Util.Run(spawnPipe, runProcessWithInput)
import XMonad.Util.EZConfig(additionalKeys)

-- Layout
import XMonad.Layout.Grid
import XMonad.Layout.Groups.Wmii
import qualified XMonad.Layout.IndependentScreens as LIS
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleDecoration

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.OnScreen
import XMonad.Actions.UpdatePointer

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

-- Main --
main = do
    nScreens <- LIS.countScreens
    -- hack from geekosaur to get stderr to a readable place
    -- closeFd 2 >> openFd ".xsession-errors" WriteOnly (Just 0644) defaultFileFlags
    -- regular code
    xmproc1 <- spawnPipe "xmobar -x 0"
    xmproc2 <- spawnPipe "xmobar -x 1"
    xmproc3 <- spawnPipe "xmobar -x 2"
    xmonad $ defaultConfig
        { manageHook = manageDocks
        , layoutHook = layoutHook'
        , terminal = "urxvt -pe selection-to-clipboard"
        , keys = keys' nScreens
        , modMask = mod4Mask
        -- xmobar
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = \a -> do hPutStrLn xmproc1 a
                                              hPutStrLn xmproc2 a
                                              hPutStrLn xmproc3 a
                        , ppLayout = \s -> ""
                        , ppHidden = xmobarColor "#D0D0D0" ""
                        , ppHiddenNoWindows = xmobarColor "#808080" ""
                        , ppTitle = xmobarColor "green" "" . shorten 120
                        } >> (local (\c -> c { mouseFocused = False }) $ updatePointer (0.5, 0.5) (0, 0))
        -- workspace setup
        , workspaces = workspaces'
        , focusFollowsMouse = False -- interacts poorly with wmii
        , handleEventHook = docksEventHook
        -- extra
        , borderWidth = 5
        }

-- Hooks --
layoutHook' = avoidStruts $
        wmii shrinkText sdTheme
    ||| noBorders Full

-- Workspaces (attached to screens, tries to be smart about how many screens there actually are)
workspaces' :: [WorkspaceId]
workspaces' = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P", "-1", "-2", "-3", "-4" ]
workspace_keys' :: [KeySym]
workspace_keys' = [
      xK_F1, xK_F2, xK_F3, xK_F4, xK_F5, xK_F6, xK_F7, xK_F8, xK_F9, xK_F10
    , xK_F11, xK_F12, xK_F13, xK_F14, xK_F15, xK_F16, xK_F17, xK_F18, xK_F19, xK_F20
    , xK_F21, xK_F22, xK_F23, xK_F24]

chooseScreen :: Int -> WorkspaceId -> ScreenId
chooseScreen 1 _ = 0
chooseScreen 2 x
    | elem x ["1", "2", "3", "4", "5", "Q", "W", "E", "R", "T" ] = 1
    | otherwise = 0
chooseScreen 3 x
    | elem x ["1", "2", "3", "4", "5", "Q", "W", "E", "R", "T" ] = 1
    | elem x ["6", "7", "8", "9", "0", "Y", "U", "I", "O", "P" ] = 2
    | otherwise = 0

viewFixedScreen :: Int -> WorkspaceId -> WindowSet -> WindowSet
viewFixedScreen nScreens wid s = viewOnScreen currentScreenId wid s
--    where currentScreenId = (W.screen . W.current) s
    where currentScreenId = chooseScreen nScreens wid


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

-- Keybindings (takes number of screens so that it can make the F-keys
--  force certain workspaces to certain monitors
keys' :: Int -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' nScreens conf = M.fromList $ [
    -- window switching
          ((XMonad.modMask conf, xK_Down), focusDown)
        , ((XMonad.modMask conf, xK_Up), focusUp)
        , ((shiftMask .|. XMonad.modMask conf, xK_Up), swapUp)
        , ((shiftMask .|. XMonad.modMask conf, xK_Down), swapDown)
        , ((XMonad.modMask conf, xK_Left), focusGroupUp)
        , ((XMonad.modMask conf, xK_Right), focusGroupDown)
        , ((shiftMask .|. XMonad.modMask conf, xK_Left), moveToGroupUp False)
        , ((shiftMask .|. XMonad.modMask conf, xK_Right), moveToGroupDown False)
        , ((XMonad.modMask conf, xK_bracketleft), zoomGroupOut)
        , ((XMonad.modMask conf, xK_bracketright), zoomGroupIn)
        , ((XMonad.modMask conf, xK_space), toggleFocusFloat)
        , ((shiftMask .|. XMonad.modMask conf, xK_space), withFocused $ windows . W.sink)
    -- screen switching
        , ((XMonad.modMask conf, xK_Tab), nextScreen)
        , ((shiftMask .|. XMonad.modMask conf, xK_Tab), shiftNextScreen)
    -- wmii and layout switching
        , ((shiftMask .|. XMonad.modMask conf, xK_minus), sendMessage NextLayout)
        , ((XMonad.modMask conf, xK_s), groupToTabbedLayout)
        , ((XMonad.modMask conf, xK_d), groupToVerticalLayout)
    -- mediakeys (nb no modmask)
        , ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl s +1%")
        , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl s 1%-")
        , ((shiftMask, xF86XK_MonBrightnessUp), spawn "brightnessctl s +10%")
        , ((shiftMask, xF86XK_MonBrightnessDown), spawn "brightnessctl s 10%-")
        , ((XMonad.modMask conf, xF86XK_Favorites), spawn "/home/apoelstra/bin/seizure.sh") -- "margot key"
    -- misc
        , ((XMonad.modMask conf, xK_BackSpace), spawn $ XMonad.terminal conf)
        , ((XMonad.modMask conf, xK_equal), spawn $ XMonad.terminal conf ++ " -e bash -c 'source ~/.bashrc && ssh camus'")
        , ((XMonad.modMask conf, xK_minus), spawn $ "xmonad --recompile && xmonad --restart")
        , ((XMonad.modMask conf, xK_minus), sendMessage ToggleStruts)
        -- shift+0 is code for ) ... unfortunately there is no shift+)
        , ((shiftMask .|. XMonad.modMask conf, xK_0), spawn $ "xmonad --recompile && xmonad --restart")
        , ((shiftMask .|. XMonad.modMask conf, xK_c), kill)
    ] ++ [
    -- workspace switching
        ((smask .|. (XMonad.modMask conf), key), windows $ fn wkspace)
            | (wkspace, key) <- zip workspaces' workspace_keys'
            , (fn, smask) <- [(viewFixedScreen nScreens, 0), (W.shift, shiftMask)]
    ]

