import           XMonad
import qualified XMonad.StackSet                   as W

import           Control.Applicative
import           Control.Monad
import qualified Data.Map                          as M
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Posix.User

import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Actions.FlexibleManipulate as Flex
import           XMonad.Actions.FloatSnap

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers

import           XMonad.Layout.BoringWindows       (boringWindows, focusDown,
                                                    focusMaster, focusUp)
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Layout.Renamed
import           XMonad.Layout.Spiral
import           XMonad.Layout.StackTile
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.WindowNavigation

import           XMonad.Layout.Grid
import           XMonad.Layout.Simplest
import           XMonad.Layout.Tabbed

import           Control.Arrow                     (first)
import           XMonad.Prompt
import           XMonad.Prompt.Input
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window

import           XMonad.Util.EZConfig
import           XMonad.Util.Run
import           XMonad.Util.WorkspaceCompare


------------------------------------------------------------------------
-- Variables

myTerminal           = "sakura --login"
myEditor             = "emacsclient --alternate-editor='' --create-frame --no-wait"
myBrowser            = "firefox"
myScreenLock         = "xscreensaver-command -lock"
myFocusFollowsMouse  = False
myClickJustFocuses   = False
myBorderWidth        = 2
myModMask            = mod4Mask
myWorkspaces         = ["*"]
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

sessionFile = "/home/yuta/.workspaces"


------------------------------------------------------------------------
-- Key bindings

myKeys conf = mkKeymap conf $
    -- Programs
    [ ("M-;",   spawn $ XMonad.terminal conf)
    , ("M-e",   spawn myEditor)
    , ("M-S-e", startEmacsDaemonPrompt myXPConfig { autoComplete = Nothing })
    , ("M-w",   spawn myBrowser)
    , ("M-C-x", spawn myScreenLock)
    -- Prompts
    , ("M-r",   shellPrompt =<< withHistMatch myXPConfig <$> initMatches)
    , ("M-g",   windowPromptGoto      myXPConfig)
    , ("M-S-b", windowPromptBring     myXPConfig)
    , ("M-C-b", windowPromptBringCopy myXPConfig)
    -- Workspaces
    , ("M-a",   addWorkspacePrompt myXPConfig)
    , ("M-v",   selectWorkspace myXPConfig)
    , ("M-S-r", renameWorkspace myXPConfig { autoComplete = Nothing })
    , ("M-S-x", removeEmptyWorkspace)
    , ("M-m",   withWorkspace myXPConfig (windows . W.shift))
    , ("M-<Page_Down>",   findWorkspace getSortByTag Next NonEmptyWS 1 >>= windows . W.greedyView)
    , ("M-<Page_Up>",     findWorkspace getSortByTag Prev NonEmptyWS 1 >>= windows . W.greedyView)
    , ("M-S-<Page_Down>", findWorkspace getSortByTag Next AnyWS 1 >>= windows . W.greedyView)
    , ("M-S-<Page_Up>",   findWorkspace getSortByTag Prev AnyWS 1 >>= windows . W.greedyView)
    , ("M-C-l", loadWorkspaces)
    , ("M-C-s", saveWorkspaces)
    -- Window manipulations
    , ("M-S-c", kill1)
    , ("M-C-S-c", killAllOtherCopies >> kill1)
    , ("M-t",   withFocused $ windows . W.sink)  -- Push window back into tiling
    , ("M-f",   withFocused (sendMessage . maximizeRestore))
    , ("M-j",   withFocused minimizeWindow)
    , ("M-k",   sendMessage RestoreNextMinimizedWin)
    -- Layout
    , ("M-<Space>",   sendMessage NextLayout)
    , ("M-S-<Space>", setLayout $ XMonad.layoutHook conf)  --  Reset the layouts on the current workspace to default
    -- Moving the focus
    , ("M-<Return>", focusMaster)
    , ("M-n",        focusDown)
    , ("M-p",        focusUp)
    , ("M-<Tab>",    windows W.focusDown)
    , ("M-S-<Tab>",  windows W.focusUp)
    -- Swapping windows
    , ("M-S-<Return>", windows W.shiftMaster)
    , ("M-S-n",        windows W.swapDown)
    , ("M-S-p",        windows W.swapUp)
    -- Sublayout
    , ("M-C-n", withFocused (sendMessage . mergeDir' W.focusDown'))
    , ("M-C-p", withFocused (sendMessage . mergeDir' W.focusUp'))
    , ("M-C-m", withFocused (sendMessage . MergeAll))
    , ("M-C-u", withFocused (sendMessage . UnMerge))
    , ("M-.",   onGroup W.focusDown')
    , ("M-,",   onGroup W.focusUp')
    -- Master area
    , ("M-h",   sendMessage Shrink)
    , ("M-l",   sendMessage Expand)
    , ("M-S-h", sendMessage (IncMasterN 1))
    , ("M-S-l", sendMessage (IncMasterN (-1)))
    -- Floating windows
    , ("M-<Left>",    withFocused $ snapMove   L Nothing)
    , ("M-<Right>",   withFocused $ snapMove   R Nothing)
    , ("M-<Up>",      withFocused $ snapMove   U Nothing)
    , ("M-<Down>",    withFocused $ snapMove   D Nothing)
    , ("M-S-<Left>",  withFocused $ snapShrink R Nothing)
    , ("M-S-<Right>", withFocused $ snapGrow   R Nothing)
    , ("M-S-<Up>",    withFocused $ snapShrink D Nothing)
    , ("M-S-<Down>",  withFocused $ snapGrow   D Nothing)
    -- Controling xmonad
    , ("M-C-q", io exitSuccess)
    , ("M-C-r", spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [("M-" ++ show n, a) | (n, a) <- zip [1..9] (map (withNthWorkspace W.greedyView) [0..])]
    ++
    [("M-S-" ++ show n, a) | (n, a) <- zip [1..9] (map (withNthWorkspace W.shift) [0..])]
    ++
    [("M-C-" ++ show n, a) | (n, a) <- zip [1..9] (map (withNthWorkspace copy) [0..])]
  where
    withHistMatch xpc hm = xpc
        { promptKeymap = M.union (M.fromList [ ((0, xK_Up),   historyUpMatching hm)
                                             , ((0, xK_Down), historyDownMatching hm)
                                             ])
                                 (promptKeymap xpc)
        }
    startEmacsDaemonPrompt xpc = do
        uid <- liftIO getRealUserID
        let socketDir = "/" </> "tmp" </> "emacs" ++ show uid
        sessions <- liftIO $ filter (`notElem` [".", ".."]) <$> getDirectoryContents socketDir
        let complFun = mkComplFunFromList' sessions
        msession <- inputPromptWithCompl xpc "session" complFun
        case msession of
            Nothing -> return ()
            Just session -> spawn $ "emacsclient --alternate-editor='' --create-frame --no-wait --socket-name='" ++ session ++ "'"

loadWorkspaces :: X ()
loadWorkspaces =
    whenX (io $ doesFileExist sessionFile) $ do
        ws <- io $ fmap lines $ readFile sessionFile
        forM_ ws addWorkspace

saveWorkspaces :: X ()
saveWorkspaces =
    withWindowSet $ \ws ->
        io $ writeFile sessionFile $ unlines $ map W.tag $ W.workspaces ws

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
            ||| Mirror (Tall nmaster delta ratio)
            ||| StackTile nmaster delta ratio
            ||| spiral (6/9)
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
    , className  =? "fontforge"       --> doFloat
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
myModifyPP pp = do
    copies <- wsContainingCopies
    let check ws | ws `elem` copies = xmobarColor "#cccc00" "" $ ws
                 | otherwise = ppHidden pp ws
    return pp { ppHidden = check }
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

statusBar' cmd pp modifyPP k conf = do
    h <- spawnPipe cmd
    return $ conf
        { layoutHook = avoidStruts (layoutHook conf)
        , logHook = do
            logHook conf
            pp' <- modifyPP pp
            dynamicLogWithPP pp' { ppOutput = hPutStrLn h }
        , manageHook = manageHook conf <+> manageDocks
        , keys = liftM2 M.union keys' (keys conf)
        }
  where
    keys' = (`M.singleton` sendMessage ToggleStruts) . k


------------------------------------------------------------------------
-- Startup hook

myStartupHook = return ()


------------------------------------------------------------------------
-- Run xmonad

main = xmonad =<< statusBar' myBar myPP myModifyPP toggleStrutsKey myConfig

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
