import XMonad
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Actions.FloatSnap

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.BoringWindows (boringWindows, focusMaster, focusUp, focusDown)
import XMonad.Layout.Fullscreen
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.Renamed
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation

import XMonad.Layout.Grid
import XMonad.Layout.Simplest
import XMonad.Layout.Tabbed

import Control.Arrow (first)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window

import XMonad.Util.WorkspaceCompare


------------------------------------------------------------------------
-- Variables

myTerminal           = "sakura -e 'zsh --login'"
myEditor             = "emacsclient --alternate-editor='' --create-frame"
myBrowser            = "google-chrome"
myScreenLock         = "xscreensaver-command -lock"
myFocusFollowsMouse  = False
myClickJustFocuses   = False
myBorderWidth        = 2
myModMask            = mod4Mask
myWorkspaces         = ["1","2","3"]
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
    , ((modm,               xK_g     ), windowPromptGoto  myXPConfig)
    , ((modm .|. shiftMask, xK_b     ), windowPromptBring myXPConfig)
    -- Workspaces
    , ((modm,               xK_a     ), addWorkspacePrompt myXPConfig)
    , ((modm,               xK_v     ), selectWorkspace myXPConfig)
    , ((modm .|. shiftMask, xK_r     ), renameWorkspace myXPConfig { autoComplete = Nothing })
    , ((modm .|. shiftMask, xK_x     ), removeEmptyWorkspace)
    , ((modm,               xK_m     ), withWorkspace myXPConfig (windows . W.shift))
    , ((modm,               xK_Page_Down), findWorkspace getSortByTag Next NonEmptyWS 1 >>= windows . W.greedyView)
    , ((modm,               xK_Page_Up  ), findWorkspace getSortByTag Prev NonEmptyWS 1 >>= windows . W.greedyView)
    , ((modm .|. shiftMask, xK_Page_Down), findWorkspace getSortByTag Next AnyWS 1 >>= windows . W.greedyView)
    , ((modm .|. shiftMask, xK_Page_Up  ), findWorkspace getSortByTag Prev AnyWS 1 >>= windows . W.greedyView)
    -- Window manipulations
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)  -- Push window back into tiling
    , ((modm,               xK_f     ), withFocused (sendMessage . maximizeRestore))
    , ((modm,               xK_j     ), withFocused minimizeWindow)
    , ((modm,               xK_k     ), sendMessage RestoreNextMinimizedWin)
    -- Layout
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)  --  Reset the layouts on the current workspace to default
    -- Moving the focus
    , ((modm,               xK_Return), focusMaster)
    , ((modm,               xK_n     ), focusDown)
    , ((modm,               xK_p     ), focusUp)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp)
    -- Swapping windows
    , ((modm .|. shiftMask, xK_Return), windows W.shiftMaster)
    , ((modm .|. shiftMask, xK_n     ), windows W.swapDown)
    , ((modm .|. shiftMask, xK_p     ), windows W.swapUp)
    -- Sublayout
    , ((modm .|. controlMask, xK_n     ), withFocused (sendMessage . mergeDir' W.focusDown'))
    , ((modm .|. controlMask, xK_p     ), withFocused (sendMessage . mergeDir' W.focusUp'))
    , ((modm .|. controlMask, xK_m     ), withFocused (sendMessage . MergeAll))
    , ((modm .|. controlMask, xK_u     ), withFocused (sendMessage . UnMerge))
    , ((modm,                 xK_period), onGroup W.focusDown')
    , ((modm,                 xK_comma ), onGroup W.focusUp')
    -- Master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm .|. shiftMask, xK_h     ), sendMessage (IncMasterN 1))
    , ((modm .|. shiftMask, xK_l     ), sendMessage (IncMasterN (-1)))
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
    , ((modm .|. controlMask, xK_q   ), io exitSuccess)
    , ((modm .|. controlMask, xK_r   ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((modm,               k), a) | (k, a) <- zip [xK_1..xK_9] (map (withNthWorkspace W.greedyView) [0..])]
    ++
    [((modm .|. shiftMask, k), a) | (k, a) <- zip [xK_1..xK_9] (map (withNthWorkspace W.shift) [0..])]

-- Modified version of `mergeDir' from XMonad.Layout.SubLayouts
mergeDir' :: (W.Stack Window -> W.Stack Window) -> Window -> GroupMsg Window
mergeDir' f = WithGroup g
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

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster)
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    , ((modm, button3), \w -> focus w >> Flex.mouseWindow Flex.resize w
                                      >> windows W.shiftMaster)
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
        ppCurrent         = xmobarColor "white" "",
        ppVisible         = wrap "(" ")",
        ppHidden          = xmobarColor "#555555" "",
        ppHiddenNoWindows = xmobarColor "#555555" "",
        ppUrgent          = xmobarColor "red" "",
        ppSep             = "  :  ",
        ppTitle           = xmobarColor "#ccff00" "" . shorten 50,
        ppLayout          = xmobarColor "white" ""
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
        clickJustFocuses   = myClickJustFocuses,
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
