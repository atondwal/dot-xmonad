import XMonad
import Data.Monoid
import System.Exit

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Renamed
import XMonad.Layout.WindowNavigation
import XMonad.Layout.BoringWindows (boringWindows, focusMaster, focusUp, focusDown)
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.Master
import XMonad.Layout.Fullscreen

import XMonad.Layout.Grid
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import Control.Arrow (first)

import XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Actions.FloatSnap

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


------------------------------------------------------------------------
-- Variables

myTerminal           = "sakura -e 'zsh --login'"
myEditor             = "gvim"
myBrowser            = "firefox"
myScreenLock         = "xscreensaver-command -lock"
myFocusFollowsMouse  = True
myBorderWidth        = 2
myModMask            = mod4Mask
myWorkspaces         = ["a","b","c","d","e","f","g","h","i"]
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
myXPConfig = defaultXPConfig {
        font          = "xft:sans-9",
        promptKeymap  = myXPKeymap,
        height        = 24,
        historyFilter = deleteAllDuplicates,
        autoComplete  = Just 500000
    }
myTheme = defaultTheme {
        fontName = "xft:sans-9"
    }


------------------------------------------------------------------------
-- Key bindings

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- Programs
    [ ((modm,                 xK_semicolon), spawn $ XMonad.terminal conf)
    , ((modm,                 xK_e        ), spawn myEditor)
    , ((modm,                 xK_w        ), spawn myBrowser)
    , ((modm .|. controlMask, xK_x        ), spawn myScreenLock)
    -- Prompts
    , ((modm,               xK_r     ), shellPrompt       myXPConfig)
    , ((modm .|. shiftMask, xK_g     ), windowPromptGoto  myXPConfig)
    , ((modm .|. shiftMask, xK_b     ), windowPromptBring myXPConfig)
    -- Workspaces
    , ((modm,               xK_v     ), selectWorkspace myXPConfig)
    , ((modm .|. shiftMask, xK_r     ), renameWorkspace myXPConfig)
    , ((modm .|. shiftMask, xK_x     ), removeEmptyWorkspace)
    , ((modm,               xK_m     ), withWorkspace myXPConfig (windows . W.shift))
    -- Window manipulations
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)  -- Push window back into tiling
    , ((modm,               xK_f     ), withFocused (sendMessage . maximizeRestore))
    , ((modm,               xK_i     ), withFocused minimizeWindow)
    , ((modm .|. shiftMask, xK_i     ), sendMessage RestoreNextMinimizedWin)
    -- Layout
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)  --  Reset the layouts on the current workspace to default
    -- Moving the focus
    , ((modm,               xK_Return), focusMaster)
    , ((modm,               xK_n     ), focusDown)
    , ((modm,               xK_p     ), focusUp)
    , ((modm,               xK_k     ), sendMessage $ Go U)
    , ((modm,               xK_j     ), sendMessage $ Go D)
    , ((modm,               xK_h     ), sendMessage $ Go L)
    , ((modm,               xK_l     ), sendMessage $ Go R)
    -- Swapping windows
    , ((modm .|. shiftMask, xK_Return), windows W.shiftMaster)
    , ((modm .|. shiftMask, xK_n     ), windows W.swapDown)
    , ((modm .|. shiftMask, xK_p     ), windows W.swapUp)
    , ((modm .|. shiftMask, xK_k     ), sendMessage $ Swap U)
    , ((modm .|. shiftMask, xK_j     ), sendMessage $ Swap D)
    , ((modm .|. shiftMask, xK_h     ), sendMessage $ Swap L)
    , ((modm .|. shiftMask, xK_l     ), sendMessage $ Swap R)
    -- Sublayout
    , ((modm .|. controlMask, xK_n     ), withFocused (sendMessage . mergeDir' W.focusDown'))
    , ((modm .|. controlMask, xK_p     ), withFocused (sendMessage . mergeDir' W.focusUp'))
    , ((modm .|. controlMask, xK_k     ), sendMessage $ pushGroup U)
    , ((modm .|. controlMask, xK_j     ), sendMessage $ pushGroup D)
    , ((modm .|. controlMask, xK_h     ), sendMessage $ pushGroup L)
    , ((modm .|. controlMask, xK_l     ), sendMessage $ pushGroup R)
    , ((modm .|. controlMask, xK_m     ), withFocused (sendMessage . MergeAll))
    , ((modm .|. controlMask, xK_u     ), withFocused (sendMessage . UnMerge))
    , ((modm .|. controlMask, xK_period), onGroup W.focusDown')
    , ((modm .|. controlMask, xK_comma ), onGroup W.focusUp')
    , ((modm,                 xK_Tab   ), onGroup W.focusDown')
    , ((modm .|. shiftMask,   xK_Tab   ), onGroup W.focusUp')
    -- Master area
    , ((modm .|. shiftMask, xK_comma ), sendMessage Shrink)
    , ((modm .|. shiftMask, xK_period), sendMessage Expand)
    , ((modm,               xK_comma ), sendMessage (IncMasterN 1))
    , ((modm,               xK_period), sendMessage (IncMasterN (-1)))
    -- Floating windows
    , ((modm,               xK_Left  ), withFocused $ snapMove   L Nothing)
    , ((modm,               xK_Right ), withFocused $ snapMove   R Nothing)
    , ((modm,               xK_Up    ), withFocused $ snapMove   U Nothing)
    , ((modm,               xK_Down  ), withFocused $ snapMove   D Nothing)
    , ((modm .|. shiftMask, xK_Left  ), withFocused $ snapShrink R Nothing)
    , ((modm .|. shiftMask, xK_Right ), withFocused $ snapGrow   R Nothing)
    , ((modm .|. shiftMask, xK_Up    ), withFocused $ snapShrink D Nothing)
    , ((modm .|. shiftMask, xK_Down  ), withFocused $ snapGrow   D Nothing)
    -- Controling xmonad
    , ((modm .|. controlMask, xK_q   ), io (exitWith ExitSuccess))
    , ((modm .|. controlMask, xK_r   ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((modm,               k), a) | (k, a) <- zip [xK_1..xK_9] (map (withNthWorkspace W.greedyView) [0..])]
    ++
    [((modm .|. shiftMask, k), a) | (k, a) <- zip [xK_1..xK_9] (map (withNthWorkspace W.shift) [0..])]
    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_o, xK_a] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- Modified version of `mergeDir' from XMonad.Layout.SubLayouts
mergeDir' :: (W.Stack Window -> W.Stack Window) -> Window -> GroupMsg Window
mergeDir' f w = WithGroup g w
  where
    g cs = do
        let c = W.focus cs
        let onlyOthersExceptCurrent = W.filter (\o -> o == c || o `notElem` W.integrate cs)
        flip whenJust (sendMessage . Merge c . W.focus . f)
            =<< fmap (onlyOthersExceptCurrent =<<) currentStack
        return cs
    currentStack :: X (Maybe (W.Stack Window))
    currentStack = gets (W.stack . W.workspace . W.current . windowset)


------------------------------------------------------------------------
-- Mouse bindings

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> Flex.mouseWindow Flex.resize w
                                       >> windows W.shiftMaster))
    ]


------------------------------------------------------------------------
-- Layouts

myLayoutHook = modifier layouts
  where
     -- layout modifiers
     modifier = renamed [CutWordsLeft 3]
              . fullscreenFull
              . configurableNavigation noNavigateBorders
              . mySubTabbed
              . maximize
              . minimize
              . boringWindows
     mySubTabbed x = addTabs shrinkText myTheme $ subLayout [] Simplest x
     -- layouts
     layouts =  Tall nmaster delta ratio
            ||| renamed [CutWordsRight 1] (GridRatio $ 4/3)
            ||| Full
     -- parameters
     nmaster = 1       -- The default number of windows in the master pane
     ratio   = 3/5     -- Default proportion of screen occupied by master pane
     delta   = 3/100   -- Percent of screen to increment by when resizing panes


------------------------------------------------------------------------
-- Window rules

myManageHook = composeAll
    [ fullscreenManageHook
    , resource   =? "desktop_window"  --> doIgnore
    , resource   =? "kdesktop"        --> doIgnore
    , className  =? "MPlayer"         --> doFloat
    , className  =? "Gnome-mplayer"   --> doFloat
    , className  =? "Gimp"            --> doFloat
    , className  =? "Gimp-2.6"        --> doFloat
    , className  =? "Audacious"       --> doFloat
    , className  =? "Uim-pref-gtk"    --> doFloat
    , className  =? "Pidgin"          --> doFloat
    , className  =? "Skype"           --> doFloat
    , className  =? "Mikutter.rb"     --> doFloat
    , chromePopupWindow               --> doFloat
    , minecraftWindow                 --> doFloat
    , firefoxDialogs                  --> doFloat
    , isDialog                        --> doCenterFloat
    ]
  where
    chromePopupWindow = className =? "Google-chrome" <&&> windowRole =? "pop-up"
    minecraftWindow = className =? "net-minecraft-LauncherFrame"
    firefoxDialogs = className =? "Firefox" <&&> resource /=? "Navigator"
    windowRole = stringProperty "WM_WINDOW_ROLE"


------------------------------------------------------------------------
-- Event handling

myEventHook = fullscreenEventHook


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
        layoutHook         = myLayoutHook,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
