{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import           XMonad
import qualified XMonad.StackSet                   as W

import           Control.Applicative
import           Control.Exception                 as E
import           Control.Monad
import           Data.List
import qualified Data.Map                          as M
import           Data.Maybe
import           Data.Word                         (Word32)
import           Foreign.C.Types                   (CLong)
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Posix.User
import           Graphics.X11.ExtraTypes.XF86

import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Actions.FlexibleManipulate as Flex
import           XMonad.Actions.FloatSnap hiding (Direction2D(..))

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks   hiding (Direction2D(..))
import           XMonad.Hooks.ManageHelpers

import           XMonad.Layout.BoringWindows       (boringWindows, focusDown,
                                                    focusMaster, focusUp)
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Layout.StackTile
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.WindowNavigation   hiding (Direction2D(..))
import           XMonad.Layout.OneBig
import           XMonad.Layout.GridVariants hiding (Orientation(..))
import qualified XMonad.Layout.GridVariants as Orientation (Orientation(..))
import           XMonad.Layout.ThreeColumns

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
import qualified XMonad.Util.Types (Direction2D)
import qualified XMonad.Util.Types as Dir2D (Direction2D(..))
import           XMonad.Util.WorkspaceCompare


------------------------------------------------------------------------
-- Variables

myTerminal           = "urxvt"
myEditor             = "emacs"
myBrowser            = "firefox"
myScreenLock         = "xscreensaver-command -lock"
myScreenOff          = "xset dpms force off"
myFocusFollowsMouse  = False
myClickJustFocuses   = False
myBorderWidth        = 0
myModMask            = mod4Mask
myWorkspaces         = ["*"]
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#272727"
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
        font          = "xft:M+ 1c:size=9",
        promptKeymap  = myXPKeymap,
        height        = 24,
        promptBorderWidth = 0,
        historyFilter = deleteAllDuplicates,
        autoComplete  = Just 500000
    }
myTheme = defaultTheme {
        fontName = "xft:M+ 1c:size=9"
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
    , ("M-C-o", spawn myScreenOff)
    , ("M-<Print>", spawn "scrot screen_%Y-%m-%d-%H-%M-%S.png")
    , ("M-C-<Print>", spawn "scrot window_%Y-%m-%d-%H-%M-%S.png -u")
    , ("<XF86MonBrightnessUp>",   spawn "xbacklight + 10")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight - 10")
    , ("<XF86AudioLowerVolume>",  spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ("<XF86AudioRaiseVolume>",  spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ("<XF86AudioMute>",         spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    -- Prompts
    , ("M-r",   shellPrompt' =<< withHistMatch myXPConfig <$> initMatches)
    , ("M-g",   windowPromptGoto      myXPConfig)
    , ("M-S-b", windowPromptBring     myXPConfig)
    , ("M-C-b", windowPromptBringCopy myXPConfig)
    -- Workspaces
    , ("M-a",   addWorkspacePrompt myXPConfig)
    , ("M-v",   selectWorkspace myXPConfig)
    , ("M-S-r", renameWorkspace myXPConfig { autoComplete = Nothing })
    , ("M-S-x", removeEmptyWorkspace)
    , ("M-m",   withWorkspace myXPConfig (windows . W.shift))
    , ("M-S-m",   withWorkspace myXPConfig { autoComplete = Nothing } (windows . W.shift))
    , ("M-<Page_Down>",   findWorkspace getSortByTag Next NonEmptyWS 1 >>= windows . W.greedyView)
    , ("M-<Page_Up>",     findWorkspace getSortByTag Prev NonEmptyWS 1 >>= windows . W.greedyView)
    , ("M-S-<Page_Down>", findWorkspace getSortByTag Next AnyWS 1 >>= windows . W.greedyView)
    , ("M-S-<Page_Up>",   findWorkspace getSortByTag Prev AnyWS 1 >>= windows . W.greedyView)
    , ("M-C-l", loadWorkspaces)
    , ("M-C-s", saveWorkspaces)
    -- Window manipulations
    , ("M-C-c", kill1)
    , ("M-C-S-c", killAllOtherCopies >> kill1)
    , ("M-t",   withFocused $ windows . W.sink)  -- Push window back into tiling
    , ("M-f",   withFocused (sendMessage . maximizeRestore))
    , ("M-j",   withFocused minimizeWindow)
    , ("M-k",   sendMessage RestoreNextMinimizedWin)
    , ("M-[",   withFocused $ fadeOut 0.1)
    , ("M-]",   withFocused $ fadeIn 0.1)
    , ("M-=",   withFocused $ setOpacity (0xfffffffe / 0xffffffff))
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
    , ("M-<Left>",    withFocused $ snapMove   Dir2D.L Nothing)
    , ("M-<Right>",   withFocused $ snapMove   Dir2D.R Nothing)
    , ("M-<Up>",      withFocused $ snapMove   Dir2D.U Nothing)
    , ("M-<Down>",    withFocused $ snapMove   Dir2D.D Nothing)
    , ("M-S-<Left>",  withFocused $ snapShrink Dir2D.R Nothing)
    , ("M-S-<Right>", withFocused $ snapGrow   Dir2D.R Nothing)
    , ("M-S-<Up>",    withFocused $ snapShrink Dir2D.D Nothing)
    , ("M-S-<Down>",  withFocused $ snapGrow   Dir2D.D Nothing)
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
        exist <- liftIO $ doesDirectoryExist socketDir
        sessions <- if exist
            then liftIO $ filter (`notElem` [".", ".."]) <$> getDirectoryContents socketDir
            else return []
        let complFun = mkComplFunFromList' sessions
        msession <- inputPromptWithCompl xpc "Emacs session" complFun
        case msession of
            Nothing -> return ()
            Just session -> spawn $ "emacsclient --alternate-editor='' --create-frame --no-wait --socket-name='" ++ session ++ "'"

shellPrompt' :: XPConfig -> X ()
shellPrompt' c = do
    cmds <- io getCommands'
    mkXPrompt Shell c (getShellCompl cmds) spawn

getCommands' :: IO [String]
getCommands' = do
    p  <- getEnv "PATH" `E.catch` econst []
    let ds = filter (/= "") $ split ':' p
    es <- forM ds $ \d -> do
        exists <- doesDirectoryExist d `E.catch` econst False
        if exists
            then getDirectoryContents d `E.catch` econst []
            else return []
    return . uniqSort . filter ((/= '.') . head) . concat $ es
  where
    econst :: Monad m => a -> IOException -> m a
    econst = const . return

rationalToOpacity :: Integral a => Rational -> a
rationalToOpacity r = round $ r * 0xffffffff

setOpacity :: Rational -> Window -> X ()
setOpacity r w = withDisplay $ \dpy -> do
    a <- getAtom "_NET_WM_WINDOW_OPACITY"
    c <- getAtom "CARDINAL"
    io $ changeProperty32 dpy w a c propModeReplace [rationalToOpacity r]

opacityToRational :: Integral a => a -> Rational
opacityToRational opacity = fromIntegral opacity / 0xffffffff

getOpacity :: Window -> X Rational
getOpacity w = withDisplay $ \dpy -> do
    a <- getAtom "_NET_WM_WINDOW_OPACITY"
    mval <- io $ getWindowProperty32 dpy a w
    return $ maybe 1 (opacityToRational . asUnsigned . head) mval
  where
    asUnsigned :: CLong -> Word32
    asUnsigned = fromIntegral

updateOpacity :: (Rational -> Rational) -> Window -> X ()
updateOpacity f w = do
    r <- getOpacity w
    let r' = max 0 $ min 1 $ f r
    setOpacity r' w

fadeOut :: Rational -> Window -> X ()
fadeOut d = updateOpacity (subtract d)

fadeIn :: Rational -> Window -> X ()
fadeIn d = updateOpacity (+ d)

loadWorkspaces :: X ()
loadWorkspaces =
    whenX (io $ doesFileExist sessionFile) $ do
        ws <- io $ fmap lines $ readFile sessionFile
        forM_ ws addWorkspace
        spawn $ unwords [ "notify-send"
                        , "xmonad"
                        , "'Loaded " ++ show (length ws) ++ " workspaces.'"]

saveWorkspaces :: X ()
saveWorkspaces =
    withWindowSet $ \ws -> do
        io $ do
            copyFile sessionFile (sessionFile ++ ".bak")
            writeFile sessionFile $ unlines $ map W.tag $ W.workspaces ws
        spawn $ unwords [ "notify-send"
                        , "xmonad"
                        , "'Current workspaces have been saved.'"
                        ]

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
     modifier = renamed [CutWordsLeft 6]
              . layoutHints
              . fullscreenFloat
              . fullscreenFocus
              . avoidStruts
              . mySubTabbed
              . spacing 4
              . smartBorders
              . configurableNavigation noNavigateBorders
              . maximize
              . minimize
              . boringWindows
     mySubTabbed x = addTabs shrinkText myTheme $ subLayout [] Simplest x
     -- layouts
     layouts =  renamed [CutWordsRight 2] (OneBig (1/2) (3/5))
            ||| SplitGrid Orientation.L 1 1 masterRatio aspectRatio resizeDelta
            ||| ThreeColMid nmaster resizeDelta (3/7)
            ||| Tall nmaster resizeDelta masterRatio
            ||| Mirror (Tall nmaster resizeDelta masterRatio)
            ||| StackTile nmaster resizeDelta masterRatio
            ||| spiral (6/9)
            ||| renamed [CutWordsRight 1] (GridRatio $ fromRational aspectRatio)
            ||| noBorders Full
     -- parameters
     nmaster     = 1     :: Int
     masterRatio = 3/5   :: Rational
     aspectRatio = 4/3   :: Rational
     resizeDelta = 3/100 :: Rational


------------------------------------------------------------------------
-- Window rules

-- The 'xprop' utility is helpful for obtaining window's properties.
myManageHook = composeAll
    [ fullscreenManageHook
    , resource   =? "desktop_window"     --> doIgnore
    , resource   =? "kdesktop"           --> doIgnore
    , className  =? "Xfce4-notifyd"      --> doIgnore
    , className  =? "MPlayer"            --> doFloat
    , className  =? "Gnome-mplayer"      --> doFloat
    , className  =? "Gimp"               --> doFloat
    , className  =? "Gimp-2.6"           --> doFloat
    , className  =? "Audacious"          --> doFloat
    , className  =? "Uim-pref-gtk"       --> doFloat
    , className  =? "Pidgin"             --> doFloat
    , className  =? "Skype"              --> doFloat
    , className  =? "Mikutter.rb"        --> doFloat
    , className  =? "fontforge"          --> doFloat
    , className  =? "Xmessage"           --> doFloat
    , className  =? "Qemu-system-x86_64" --> doFloat
    , className  =? "Remote-viewer"      --> doFloat
    , windowRole =? "pop-up"             --> doFloat
    , minecraftWindow                    --> doFloat
    , firefoxDialogs                     --> doFloat
    , isDialog                           --> doCenterFloat
    ]
  where
    minecraftWindow = className =? "net-minecraft-bootstrap-Bootstrap"
                    <||> ("Minecraft" `isPrefixOf`) <$> title
    firefoxDialogs = className =? "Firefox" <&&> resource /=? "Navigator"
    windowRole = stringProperty "WM_WINDOW_ROLE"


------------------------------------------------------------------------
-- Event handling

myEventHook = hintsEventHook <+> fullscreenEventHook


------------------------------------------------------------------------
-- Status bars and logging

myLogHook = return ()
myBar = "LANG=ja_JP.UTF-8 xmobar"
myPP = xmobarPP {
        ppCurrent         = xmobarColor "#1f2329" "#e4c374" . wrap " " " ",
        ppVisible         = wrap "(" ")",
        ppHidden          = xmobarColor "#aeb8c5" "" . wrap " " " ",
        ppHiddenNoWindows = xmobarColor "#626972" "" . wrap " " " ",
        ppUrgent          = xmobarColor "#ffffff" "#b05353" . wrap " " " ",
        ppSep             = "  ",
        ppWsSep           = "",
        ppTitle           = xmobarColor "#ecf4ff" "" . wrap " " " ",
        ppLayout          = xmobarColor "#1f2329" "#887cb2" . wrap " " " "
    }
myModifyPP pp = do
    wsWithCopies <- wsContainingCopies
    wsWithWindows <- gets $ map W.tag . filter (isJust . W.stack) . W.workspaces . windowset
    let check ws | ws `elem` wsWithCopies = xmobarColor "#88b8b0" "" ws
                 | ws `elem` wsWithWindows = ppHidden pp ws
                 | otherwise = ppHiddenNoWindows pp ws
    return pp { ppHidden = check }
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

statusBar' cmd pp modifyPP k conf = do
    h <- spawnPipe cmd
    return $ conf
        { logHook = do
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
