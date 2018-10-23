module Main where

import           Brick.AttrMap               (AttrMap, attrMap, attrName)
import           Brick.BChan                 (BChan, newBChan, writeBChan)
import           Brick.Main                  (App (App), appAttrMap,
                                              appChooseCursor, appDraw,
                                              appHandleEvent, appStartEvent,
                                              continue, customMain, halt,
                                              showFirstCursor, suspendAndResume)
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

-- @returns the next GameState (wrapped in a Brick EventM).
appEvent :: GameState -> BrickEvent () Tick -> EventM () (Next GameState)
appEvent s (AppEvent Tick)           = continue $ tick s
appEvent s (VtyEvent (V.EvKey k [])) = appEventKey s k
appEvent s _                         = continue $ redraw s

-- Helper function for appEvent, which only processes characters.
appEventKey :: GameState -> Key -> EventM () (Next GameState)
appEventKey s k
  | k == V.KEsc   || k == V.KChar 'q'  = halt s
  | k == V.KUp    || k == V.KChar 'w'  = continue $ redraw $ move N s
  | k == V.KDown  || k == V.KChar 's'  = continue $ redraw $ move S s
  | k == V.KLeft  || k == V.KChar 'a'  = continue $ redraw $ move W s
  | k == V.KRight || k == V.KChar 'd'  = continue $ redraw $ move E s
  | k == V.KChar ' '                   = continue $ redraw $ spin s
  | k == V.KChar '1'                   = if s ^.over
                                           then suspendAndResume
                                                $ redraw <$> mkInitState Easy
                                           else continue (redraw s)
  | k == V.KChar '2'                   = if s ^.over
                                           then suspendAndResume
                                                $ redraw <$> mkInitState Mid
                                           else continue (redraw s)
  | k == V.KChar '3'                   = if s ^.over
                                           then suspendAndResume
                                                $ redraw <$> mkInitState Hard
                                           else continue (redraw s)
  | otherwise                          = continue $ redraw s

-- @return initialized tick thread at getFrameRate (encapsulated in delayVar)
controlThread :: TVar Int -> BChan Tick -> IO ()
controlThread delay chan = forever $ do
  writeBChan chan Tick
  ms <- atomically $ readTVar delay
  threadDelay ms

main :: IO ()
main = do
  let init_vty = V.mkVty =<< V.standardIOConfig
  let init_app = App { appDraw         = UI.render
                     , appChooseCursor = showFirstCursor
                     , appHandleEvent  = appEvent
                     , appStartEvent   = return
                     , appAttrMap      = const
                                       $ attrMap V.defAttr
                                       [ (attrName "fg-blue", fg V.blue)
                                       , (attrName "fg-red", fg V.red)
                                       , (attrName "fg-white", fg V.white)
                                       , (attrName "bg-red", bg V.red)
                                       ]
                     } :: App GameState Tick ()

  chan     <- newBChan getBChanQueueLength
  delayVar <- atomically $ newTVar getFrameRate
  void $ forkIO $ controlThread delayVar chan

  init_state <- redraw <$> mkInitState Easy
  void $ customMain init_vty (Just chan) init_app init_state
