import XMonad
import XMonad.Config.Kde
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.CycleWS as Cy
import XMonad.Actions.CycleWindows as Cw
import XMonad.Actions.WindowGo as Go
import qualified XMonad.StackSet as W

import qualified Data.Map as M
import qualified Data.Char as C



------------------------------------------------------------------------------------------------
---- Configuration -----------------------------------------------------------------------------

k_monitorCount = 3

main = xmonad $ ewmh $ kde4Config
    {   modMask = mod4Mask
    ,   manageHook = manageDocks <+> myManageHook <+> manageHook kde4Config
    ,   keys = \c -> mykeys c `M.union` keys kde4Config c
    ,   layoutHook = avoidStruts (layoutHook kde4Config)
    ,   startupHook = startupHook kde4Config >> myStartUpHook
    ,   borderWidth = 1
    ,   focusedBorderColor = "#456def"
    ,   logHook = myLogHook
    ,   workspaces = myWorkspaces
    }
    

    
myWorkspaces = map show [1 .. 9 :: Int]
    
mykeys (XConfig {modMask = modm}) = M.fromList $ 
    [     -- dmenu
          ((modm,                 xK_o), spawn "dmenu_run")  
        --debug
        , ((modm .|. shiftMask, xK_g), getNuberOfMonitors )
        
        -- Workspace Cycling
        , ((modm,               xK_Up),     Cy.nextWS)
        , ((modm,               xK_Down),   Cy.prevWS)
        , ((modm .|. shiftMask, xK_Up),     Cy.shiftToNext >> Cy.nextWS)
        , ((modm .|. shiftMask, xK_Down),   Cy.shiftToPrev >> Cy.prevWS)
        , ((modm,               xK_Left),   Cy.nextScreen)
        , ((modm,               xK_Right),  Cy.prevScreen)
        , ((modm .|. shiftMask, xK_Left),   Cy.shiftNextScreen >> Cy.nextScreen)
        , ((modm .|. shiftMask, xK_Right),  Cy.shiftPrevScreen >> Cy.prevScreen)
        
        
        -- cycling through windows in 
        , ((modm,               xK_z     ), windows W.focusDown)
        , ((modm,               xK_a     ), windows W.focusUp  )
        , ((modm .|. shiftMask, xK_z     ), windows W.swapDown  )
        , ((modm .|. shiftMask, xK_a     ), windows W.swapUp    )
        
        -- Other commands
        , ((mod1Mask, xK_Tab), Cw.cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab)
        
         -- TODO Add swap left right workspaces in screens
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces kde4Config) [xK_1 .. xK_9] -- Make sure that this is divisible
                                                                      -- by the number of monitors
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]] -- W.view to not move
    where 
        
        
        -- tripleNext = Cy.nextWS >> Cy.nextWS >> Cy.nextWS
        -- triplePrev = Cy.prevWS >> Cy.prevWS >> Cy.prevWS
        -- tripleShiftToNext = Cy.shiftToNext >> Cy.shiftToNext >> Cy.shiftToNext
        -- tripleShiftToPrev = Cy.shiftToPrev >> Cy.shiftToPrev >> Cy.shiftToPrev
    
   
raiseVar :: IO String -> X ()
raiseVar getvar = liftIO getvar >>= \var -> Go.runOrRaise var (fmap (map C.toLower) className =? var)

-- Should probably figure out a way to find this names as they come out
myManageHook = composeAll  
  [ className =? "yakuake" --> doFloat  
  , className =? "Yakuake" --> doFloat  
  , className =? "Kmix" --> doFloat  
  , className =? "kmix" --> doFloat  
  , className =? "plasma" --> doFloat  
  , className =? "Plasma" --> doFloat  
  , className =? "plasma-desktop" --> doFloat  
  , className =? "Plasma-desktop" --> doFloat  
  , className =? "ksplashsimple" --> doFloat  
  , className =? "ksplashqml" --> doFloat  
  , className =? "ksplashx" --> doFloat
  , className =? "plasmashell" --> doFloat
  , className =? "ksmserver" --> doFloat
  , className =? "krunner" --> doIgnore >> doFloat
  , isKDETrayWindow         --> doIgnore >> doFloat
  , isDialog                --> doIgnore >> doFloat
  ] 
   

   
myStartUpHook = do
    return ()
   
myLogHook = do
    return ()
    
    
------------------------------------------------------------------------------------------------
---- Helper functions --------------------------------------------------------------------------
    
   
getNuberOfMonitors :: X ()
getNuberOfMonitors = do
    ws <- gets windowset
    liftIO $ print $ ws
   
