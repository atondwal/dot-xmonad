import XMonad
import Data.Monoid
import System.Exit

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Master
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Grid
import XMonad.Layout.Minimize
import XMonad.Layout.LayoutModifier

import XMonad.Prompt
import XMonad.Prompt.Shell
import Control.Arrow (first)

import qualified XMonad.Actions.FlexibleManipulate as Flex

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


------------------------------------------------------------------------
-- Variables

myTerminal           = "sakura -e 'zsh --login'"
myFocusFollowsMouse  = True
myBorderWidth        = 1
myModMask            = mod4Mask
myWorkspaces         = ["1","2","3","4","5","6","7","8","9"]
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"
myXPKeymap           = myXPKeymap' `M.union` defaultXPKeymap
    where
        myXPKeymap' = M.fromList $
            map (first $ (,) controlMask)
            [ (xK_m, setSuccess True >> setDone True)
            , (xK_h, deleteString Prev)
            , (xK_d, deleteString Next)
            , (xK_b, moveCursor Prev)
            , (xK_f, moveCursor Next)
            , (xK_p, moveHistory W.focusDown')
            , (xK_n, moveHistory W.focusUp')
            ]
myXPConfig           = defaultXPConfig {
        font          = "xft:sans-9",
        promptKeymap  = myXPKeymap,
        height        = 24,
        historyFilter = deleteAllDuplicates,
        autoComplete  = Just 500000
    }


------------------------------------------------------------------------
-- Key bindings

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm,               xK_semicolon), spawn $ XMonad.terminal conf)
    -- Open the shell prompt
    , ((modm,               xK_r     ), shellPrompt myXPConfig)

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    -- Minimize/Restore
    , ((modm,               xK_m     ), withFocused minimizeWindow)
    , ((modm .|. shiftMask, xK_m     ), sendMessage RestoreNextMinimizedWin)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm .|. shiftMask, xK_r     ), refresh)

    -- Move focus to the next/previous window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp)
    , ((modm,               xK_n     ), windows W.focusDown)
    , ((modm,               xK_p     ), windows W.focusUp)
    -- Swap the focused window with the next/previous window
    , ((modm .|. shiftMask, xK_n     ), windows W.swapDown)
    , ((modm .|. shiftMask, xK_p     ), windows W.swapUp)
    -- Move focus to the neighbor window
    , ((modm,               xK_k     ), sendMessage $ Go U)
    , ((modm,               xK_j     ), sendMessage $ Go D)
    , ((modm,               xK_h     ), sendMessage $ Go L)
    , ((modm,               xK_l     ), sendMessage $ Go R)
    -- Swap the focused window with the neighbor window
    , ((modm .|. shiftMask, xK_k     ), sendMessage $ Swap U)
    , ((modm .|. shiftMask, xK_j     ), sendMessage $ Swap D)
    , ((modm .|. shiftMask, xK_h     ), sendMessage $ Swap L)
    , ((modm .|. shiftMask, xK_l     ), sendMessage $ Swap R)

    -- Move focus to the master window
    , ((modm,               xK_Return), windows W.focusMaster  )
    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)
    -- Shrink the master area
    , ((modm .|. shiftMask, xK_comma ), sendMessage Shrink)
    -- Expand the master area
    , ((modm .|. shiftMask, xK_period), sendMessage Expand)
    -- Increment the number of windows in the master area
    , ((modm,               xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm,               xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modm .|. controlMask, xK_q   ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modm .|. controlMask, xK_r   ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> Flex.mouseWindow Flex.resize w
                                       >> windows W.shiftMaster))
    ]


------------------------------------------------------------------------
-- Layouts

myLayout = tiled ||| masteredGrid ||| grid ||| Full
  where
     -- tiling algorithms
     tiled        = configurableNavigation noNavigateBorders $ minimize $ Tall nmaster delta ratio
     grid         = configurableNavigation noNavigateBorders $ minimize $ Grid
     masteredGrid = configurableNavigation noNavigateBorders $ minimize $ mastered delta ratio $ Grid

     nmaster = 1      -- The default number of windows in the master pane
     ratio   = 1/2    -- Default proportion of screen occupied by master pane
     delta   = 3/100  -- Percent of screen to increment by when resizing panes


------------------------------------------------------------------------
-- Window rules

myManageHook = composeAll
    [ resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "MPlayer"        --> doFloat
    , className =? "Gnome-mplayer"  --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Audacious"      --> doFloat
    , className =? "Pidgin"         --> doFloat
    , className =? "Skype"          --> doFloat
    , (className =? "Firefox" <&&> resource =? "Places") --> doFloat
    , (className =? "Firefox" <&&> resource =? "Dialog") --> doCenterFloat
    , isDialog                      --> doCenterFloat
    ]


------------------------------------------------------------------------
-- Event handling

myEventHook = mempty


------------------------------------------------------------------------
-- Status bars and logging

myLogHook = return ()
myBar = "xmobar"
myPP = xmobarPP {
        ppCurrent         = xmobarColor "yellow" "" . wrap "[" "]",
        ppVisible         = wrap "(" ")",
        ppHidden          = xmobarColor "yellow" "",
        ppHiddenNoWindows = id,
        ppTitle           = xmobarColor "green" "" . shorten 50
    }
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)


------------------------------------------------------------------------
-- Startup hook

myStartupHook = return ()


------------------------------------------------------------------------
-- Run xmonad

main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

myConfig = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
