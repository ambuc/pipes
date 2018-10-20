module Main where

import           Brick.AttrMap               (AttrMap, attrMap, attrName)
import           Brick.BChan                 (BChan, newBChan, writeBChan)
import           Brick.Main                  (App (App), appAttrMap,
                                              appChooseCursor, appDraw,
                                              appHandleEvent, appStartEvent,
                                              continue, customMain, halt,
                                              showFirstCursor)
import           Brick.Types                 (BrickEvent (AppEvent, VtyEvent),
                                              EventM, Next, Widget)
import           Brick.Util                  (bg, fg, on)
import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar)
import           Control.Monad               (forever, void)
import           Control.Monad.STM           (atomically)
import           Graphics.Vty                as V
import           Lens.Micro                  ((^.))
import           System.Random               as R

import           Actions                     (move, spin, tick)
import           Init                        (mkInitState)
import           Magic                       (getBChanQueueLength, getFrameRate)
import           Types
import           UI                          (redraw, render)

data Tick = Tick

appEvent :: GameState
         -> BrickEvent () Tick
         -> EventM () (Next GameState)
appEvent s (AppEvent Tick)                       = continue $ tick s
appEvent s (VtyEvent (V.EvKey V.KEsc []))        = halt s
appEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
appEvent s (VtyEvent (V.EvKey V.KUp []))         = continue $ redraw $ move N s
appEvent s (VtyEvent (V.EvKey V.KDown []))       = continue $ redraw $ move S s
appEvent s (VtyEvent (V.EvKey V.KLeft []))       = continue $ redraw $ move W s
appEvent s (VtyEvent (V.EvKey V.KRight []))      = continue $ redraw $ move E s
appEvent s (VtyEvent (V.EvKey (V.KChar 'w') [])) = continue $ redraw $ move N s
appEvent s (VtyEvent (V.EvKey (V.KChar 's') [])) = continue $ redraw $ move S s
appEvent s (VtyEvent (V.EvKey (V.KChar 'a') [])) = continue $ redraw $ move W s
appEvent s (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue $ redraw $ move E s
appEvent s (VtyEvent (V.EvKey (V.KChar ' ') [])) = continue $ redraw $ spin s
appEvent s _                                     = continue $ redraw s

aMap :: AttrMap
aMap = attrMap V.defAttr
     [ (attrName "bold", withStyle defAttr bold)
     , (attrName "bg-blue", bg V.blue)
     , (attrName "fg-blue", fg V.blue)
     , (attrName "bg-red", bg V.red)
     , (attrName "fg-red", fg V.red)
     ]

mkApp :: App GameState Tick ()
mkApp = App { appDraw         = UI.render
            , appChooseCursor = showFirstCursor
            , appHandleEvent  = appEvent
            , appStartEvent   = return
            , appAttrMap      = const aMap
            }

controlThread :: TVar Int -> BChan Tick -> IO ()
controlThread delay chan = forever $ do
  writeBChan chan Tick
  ms <- atomically $ readTVar delay
  threadDelay ms

main :: IO ()
main = do
  chan <- newBChan getBChanQueueLength
  delayVar <- atomically $ newTVar getFrameRate
  void $ forkIO $ controlThread delayVar chan

  let init_vty = V.mkVty =<< V.standardIOConfig
  let init_app = mkApp
  init_state <- redraw <$> mkInitState
  void $ customMain init_vty (Just chan) init_app init_state
