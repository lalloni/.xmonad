import Data.Maybe
import Data.Ratio ((%))

import XMonad
import XMonad.Actions.Commands
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowMenu
import XMonad.Config.Xfce
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
import XMonad.Operations
import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.Themes
import qualified XMonad.StackSet as W

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

modm = mod4Mask
moda = mod1Mask

baseConfig = xfceConfig

main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $ baseConfig { modMask    = modm
                        , workspaces = myWorkspaces
                        , layoutHook = myLayoutHook
                        , manageHook = myManageHook
                        , logHook    = myLogHook dbus
                        } 
                        `additionalKeys` (
                            [ ((moda,                 xK_Tab),   windows W.focusDown)
                            , ((modm .|. controlMask, xK_space), sendMessage NextLayout)
                            , ((modm,                 xK_Right), nextWS)
                            , ((modm,                 xK_Left),  prevWS)
                            , ((modm .|. shiftMask,   xK_Right), shiftToNext >> nextWS)
                            , ((modm .|. shiftMask,   xK_Left),  shiftToPrev >> prevWS)
                            , ((mod4Mask,             xK_s),     cycleRecentWindows [xK_Super_L] xK_s xK_w)
                            , ((modm .|. shiftMask,   xK_g),     gotoMenuArgs ["-b", "-l", "10"])
                            , ((modm .|. shiftMask,   xK_b),     bringMenuArgs ["-b", "-l", "10"])
                            , ((moda,                 xK_space), windowMenu)
                            , ((modm .|. controlMask, xK_x),     xmonadPrompt defaultXPConfig)
                            , ((modm .|. controlMask, xK_y),     shellPrompt defaultXPConfig)
                            , ((moda,                 xK_F4),    kill)
                            ] 
                            ++  [ ((m .|. modm, k), windows $ f i) 
                                | (i, k) <- zip myWorkspaces ([xK_1 .. xK_9] ++ [xK_0])
                                , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
                                ]
                            )
                        `removeKeys` [ (modm, xK_space) -- for kupfer
                                     ]
    where
        getWellKnownName dbus = do
            D.requestName dbus (D.busName_ "org.xmonad.Log") [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
            return ()

myWorkspaces = ["dev", "mail"] ++ map show [1..6] ++ ["im", "log"]

myLayoutHook = avoidStruts $ smartBorders $ onWorkspace "im" im $ full ||| tiled ||| Mirror tiled
    where
        full = tabbed shrinkText $ theme myTheme
        tiled = Tall 1 (5/100) (1/2)
        im = reflectHoriz $ withIM (1%5) (Role "buddy_list") Grid ||| Full

myManageHook = composeAll [ className =? "Pidgin" --> doShift "im"
                          , className =? "Eclipse" --> doShift "dev"
                          , className =? "Thunderbird" --> doShift "mail"
                          , className =? "Xfce4-notifyd" --> doIgnore
                          ] <+> manageHook baseConfig

myTheme = TI { themeName = "myTheme"
             , theme     = defaultTheme { activeColor         = "#cccccc"
                                        , inactiveColor       = "#222222"
                                        , activeBorderColor   = "#cccccc"
                                        , inactiveBorderColor = "#222222"
                                        , activeTextColor     = "#222222"
                                        , inactiveTextColor   = "#cccccc"
                                        , decoHeight          = 14
                                        , fontName            = "xft:Ubuntu-9:light"
                                        , urgentColor         = "#882222"
                                        , urgentTextColor     = "#ffffff"
                                        }
             }

myLogHook dbus = dynamicLogWithPP (prettyPrinter dbus)
    where
        prettyPrinter dbus = defaultPP { ppOutput   = dbusOutput dbus
                                       , ppTitle    = pangoSanitize
                                       , ppCurrent  = pangoColor "green" . wrap "[" "]" . pangoSanitize
                                       , ppVisible  = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
                                       , ppHidden   = const ""
                                       , ppUrgent   = pangoColor "red"
                                       , ppLayout   = const ""
                                       , ppSep      = " "
                                       }
        dbusOutput dbus str = do
            let object = fromJust $ D.parseObjectPath "/org/xmonad/Log"
            let interface = fromJust $ D.parseInterfaceName "org.xmonad.Log"
            let member = fromJust $ D.parseMemberName "update"
            let signal = (D.signal object interface member) { D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")] }
            D.emit dbus signal
        pangoColor fg = wrap left right
            where
                left  = "<span foreground=\"" ++ fg ++ "\">"
                right = "</span>"
        pangoSanitize = foldr sanitize ""
            where
                sanitize '>'  xs = "&gt;" ++ xs
                sanitize '<'  xs = "&lt;" ++ xs
                sanitize '\"' xs = "&quot;" ++ xs
                sanitize '&'  xs = "&amp;" ++ xs
                sanitize x    xs = x:xs
