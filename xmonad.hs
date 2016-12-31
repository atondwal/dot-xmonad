{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import           XMonad                            hiding ((|||))
import qualified XMonad.StackSet                   as W

import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Actions.FlexibleManipulate as Flex
import           XMonad.Actions.FloatSnap          hiding (Direction2D (..))
import           XMonad.Actions.PhysicalScreens    (onNextNeighbour)

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops         (ewmh)
import           XMonad.Hooks.ManageDocks          hiding (Direction2D (..))
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName

import           XMonad.Layout.BoringWindows       (boringWindows, focusDown,
                                                    focusMaster, focusUp)
import           XMonad.Layout.CenteredMaster
import           XMonad.Layout.Circle
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Grid
import           XMonad.Layout.GridVariants        hiding (Orientation (..))
import qualified XMonad.Layout.GridVariants        as Orientation (Orientation (..))
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.OneBig
import           XMonad.Layout.Renamed
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Layout.StackTile
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.WindowNavigation    hiding (Direction2D (..))

import           XMonad.Prompt
import           XMonad.Prompt.Input
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window

import           XMonad.Util.EZConfig
import           XMonad.Util.Run
import qualified XMonad.Util.Types                 (Direction2D)
import qualified XMonad.Util.Types                 as Dir2D (Direction2D (..))
import           XMonad.Util.WorkspaceCompare

import           Control.Applicative
import           Control.Arrow                     (first)
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Exception                 as E
import           Control.Monad
import           Data.Char                         (toLower)
import           Data.List
import qualified Data.Map                          as M
import           Data.Maybe
import           Data.Word                         (Word32)
import           Foreign.C.Error                   (Errno (..), ePIPE)
import           Foreign.C.Types                   (CLong)
import           GHC.IO.Exception                  (IOErrorType (..),
                                                    IOException (..))
import           Graphics.X11.ExtraTypes.XF86
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Unsafe                  (unsafePerformIO)
import           System.Posix.User
import           System.Process
import           System.Taffybar.Hooks.PagerHints  (pagerHints)


------------------------------------------------------------------------
-- Variables

dpi :: Double
dpi = unsafePerformIO $ read <$> do
    mdpiStr <- lookup "DPI" <$> getEnvironment
    case mdpiStr of
        Just dpiStr -> return dpiStr
        Nothing -> do
            let commandLine = "xdpyinfo | sed -n '/resolution/ { s/^[^0-9]*\\([0-9]\\+\\)x.*$/\\1/; p; }'"
            readCreateProcess (shell commandLine) ""

myTerminal           = "~/dotfiles/launch-st -- zsh --login"
myEditor             = "~/dotfiles/launch-st -e nvim"
myBrowser            = "firefox"
myScreenOff          = "sleep 0.5; xset dpms force off"
myScreenLock         = "slock" ++ " " ++ myScreenOff
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
        font          = "xft:Monospace:size=9",
        bgColor       = "#f8f8f8",
        fgColor       = "#383838",
        searchPredicate = \input candidate -> all (`isInfixOf` candidate) (words input),
        promptKeymap  = myXPKeymap,
        height        = floor $ 22 * dpi / 96,
        promptBorderWidth = 0,
        historyFilter = deleteAllDuplicates,
        autoComplete  = Just (millisecond 500)
    }
  where
    millisecond = (* 1000)
myTheme = defaultTheme {
        fontName = "xft:M+ 1c:size=9",
        decoHeight = floor $ 22 * dpi / 96
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
    , ("M-<Print>", do
          spawn "scrot screen_%Y-%m-%d-%H-%M-%S.png"
          spawn "notify-send --icon=shutter --expire-time=1500 xmonad 'Screen captured.'")
    , ("M-C-<Print>", do
          spawn "scrot window_%Y-%m-%d-%H-%M-%S.png -u"
          spawn "notify-send --icon=shutter --expire-time=1500 xmonad 'Window captured.'")
    , ("<XF86MonBrightnessUp>",   spawn "xbacklight + 10")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight - 10")
    , ("<XF86AudioLowerVolume>",  spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ("<XF86AudioRaiseVolume>",  spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ("<XF86AudioMute>",         spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    -- Prompts
    , ("M-r",   shellPrompt' =<< withHistMatch myXPConfig { searchPredicate = isPrefixOf } <$> initMatches)
    , ("M-g",   windowPromptGoto      myXPConfig)
    , ("M-S-b", windowPromptBring     myXPConfig)
    , ("M-C-b", windowPromptBringCopy myXPConfig)
    , ("M-c",   commandPrompt myXPConfig { font = "xft:Monospace:size=9" })
    -- Workspaces
    , ("M-a",   addWorkspacePrompt myXPConfig)
    , ("M-v",   selectWorkspace myXPConfig)
    , ("M-S-r", renameWorkspace myXPConfig { autoComplete = Nothing })
    , ("M-S-x", removeEmptyWorkspace)
    , ("M-m",   withWorkspace myXPConfig (windows . W.shift))
    , ("M-S-m",   withWorkspace myXPConfig { autoComplete = Nothing } (windows . W.shift))
    , ("M-<Page_Down>",   findWorkspace getSortByTag Next HiddenNonEmptyWS 1 >>= windows . W.greedyView)
    , ("M-<Page_Up>",     findWorkspace getSortByTag Prev HiddenNonEmptyWS 1 >>= windows . W.greedyView)
    , ("M-C-<Page_Down>", findWorkspace getSortByTag Next (WSTagGroup '/') 1 >>= windows . W.greedyView)
    , ("M-C-<Page_Up>",   findWorkspace getSortByTag Prev (WSTagGroup '/') 1 >>= windows . W.greedyView)
    , ("M-S-<Page_Down>", findWorkspace getSortByTag Next AnyWS 1 >>= windows . W.greedyView)
    , ("M-S-<Page_Up>",   findWorkspace getSortByTag Prev AnyWS 1 >>= windows . W.greedyView)
    , ("M-C-l", loadWorkspaces)
    , ("M-C-s", saveWorkspaces)
    , ("M-s", onNextNeighbour W.view)
    , ("M-S-s", onNextNeighbour W.shift)
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
    , ("M-b", sendMessage ToggleStruts)
    , ("M-<Space>", inputPromptWithCompl  myXPConfig "Layout" (mkComplFunFromListIgnoreCase' myLayoutNames) ?+ (sendMessage . JumpToLayout))
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
    , ("M-S-h", sendMessage (IncMasterRows 1)    >> sendMessage (IncMasterN 1))
    , ("M-S-l", sendMessage (IncMasterRows (-1)) >> sendMessage (IncMasterN (-1)))
    , ("M-S-v", sendMessage (IncMasterCols 1))
    , ("M-S-u", sendMessage (IncMasterCols (-1)))
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
    , ("M-C-r", do
        spawn $ unwords [ "notify-send"
                        , "--icon=system-reboot"
                        , "--expire-time=3000"
                        , "xmonad"
                        , "Restarting..."]
        spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [("M-" ++ show n, a) | (n, a) <- zip [1..9] (map (withNthWorkspace W.greedyView) [0..])]
    ++
    [("M-S-" ++ show n, a) | (n, a) <- zip [1..9] (map (withNthWorkspace W.shift) [0..])]
    ++
    [("M-C-" ++ show n, a) | (n, a) <- zip [1..9] (map (withNthWorkspace copy) [0..])]
    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [ ("M-" ++ mod ++ key, screenWorkspace screen >>= flip whenJust (windows . func))
      | (key, screen) <- zip ["<F" ++ show n ++ ">" | n <- [1..3]] [0..]
      , (func, mod) <- [(W.view, ""), (W.shift, "S-")]]

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
            Just input -> spawn $ "emacsclient --alternate-editor='' --create-frame --no-wait --socket-name='" ++ session ++ "'"
              where
                session | input == "" = "server"
                        | otherwise   = input

mkComplFunFromListIgnoreCase' :: [String] -> String -> IO [String]
mkComplFunFromListIgnoreCase' l [] = return l
mkComplFunFromListIgnoreCase' l s =
    return $ filter ((s' `isPrefixOf`) . map toLower) l
  where
    s' = map toLower s

shellPrompt' :: XPConfig -> X ()
shellPrompt' c = do
    cmds <- io getCommands'
    mkXPrompt Shell c (getShellCompl cmds $ searchPredicate c) spawn

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

withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `E.onException` killThread tid

ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = E.handle $ \e -> case e of
                                   IOError { ioe_type  = ResourceVanished
                                           , ioe_errno = Just ioe }
                                     | Errno ioe == ePIPE -> return ()
                                   _ -> throwIO e

waitForProcess' :: ProcessHandle -> IO ExitCode
waitForProcess' ph = do
    mec <- getProcessExitCode ph
    case mec of
        Nothing -> yield >> waitForProcess' ph
        Just ec -> return ec

commandPrompt config =
    inputPrompt config "Command" ?+ (io . runNotify)
  where
    runNotify commandline = do
        (Just hin, Just hout, _, ph) <- do
            (rend, wend) <- createPipe
            (Just hin, _, _, ph) <- createProcess (shell commandline)
                { std_in  = CreatePipe
                , std_out = UseHandle wend
                , std_err = UseHandle wend
                }
            return (Just hin, Just rend, Just rend, ph)
        output <- hGetContents hout
        withForkWait (E.evaluate $ rnf output) $ \waitOutput -> do
            ignoreSigPipe $ hClose hin
            waitOutput
            hClose hout
        _ <- waitForProcess' ph
        safeSpawn "notify-send" ["--icon=terminal", "--expire-time=60000", commandline, output]

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
        ws <- io $ lines <$> readFile sessionFile
        forM_ ws addWorkspace
        spawn $ unwords [ "notify-send"
                        , "--icon=workspace-switcher"
                        , "xmonad"
                        , "'Loaded " ++ show (length ws) ++ " workspaces.'"]

saveWorkspaces :: X ()
saveWorkspaces =
    withWindowSet $ \ws -> do
        io $ do
            copyFile sessionFile (sessionFile ++ ".bak")
            writeFile sessionFile $ unlines $ map W.tag $ W.workspaces ws
        spawn $ unwords [ "notify-send"
                        , "--icon=workspace-switcher"
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

myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
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
     layouts =  renamed [Replace "OneBig"    ] (Mirror $ OneBig (3/5) (1/2))
            ||| renamed [Replace "Center"    ] (centerMaster $ Mirror $ SplitGrid Orientation.T 1 0 masterRatio (recip aspectRatio) resizeDelta)
            ||| renamed [Replace "Grid"      ] (Mirror $ SplitGrid Orientation.T 1 0 masterRatio (recip aspectRatio) resizeDelta)
            ||| renamed [Replace "Circle"    ] (Circle)
            ||| renamed [Replace "ThreeCol"  ] (ThreeColMid nmaster resizeDelta (3/7))
            ||| renamed [Replace "Tall"      ] (Tall nmaster resizeDelta masterRatio)
            ||| renamed [Replace "MirrorTall"] (Mirror $ Tall nmaster resizeDelta masterRatio)
            ||| renamed [Replace "Spiral"    ] (spiral (6/9))
            ||| renamed [Replace "Full"      ] (noBorders Full)
     -- parameters
     nmaster     = 1     :: Int
     masterRatio = 3/5   :: Rational
     aspectRatio = 4/3   :: Rational
     resizeDelta = 3/100 :: Rational
myLayoutNames =
    [ "OneBig"
    , "Center"
    , "Grid"
    , "Circle"
    , "ThreeCol"
    , "Tall"
    , "MirrorTall"
    , "Spiral"
    , "Full"
    ]


------------------------------------------------------------------------
-- Window rules

-- The 'xprop' utility is helpful for obtaining window's properties.
myManageHook = composeAll
    [ manageDocks
    , fullscreenManageHook
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
    , className  =? "xfreerdp"           --> doFloat
    , appName    =? "crx_knipolnnllmklapflnccelgolnpehhpl" --> doFloat
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


------------------------------------------------------------------------
-- Startup hook

myStartupHook = setWMName "LG3D"


------------------------------------------------------------------------
-- Run xmonad

main = do
    spawn "killall --regexp taffybar-; taffybar"
    spawn "killall compton; sleep 0.1; compton"
    xmonad (ewmh $ pagerHints myConfig)

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
