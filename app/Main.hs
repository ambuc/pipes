module Main where

import           Brick.AttrMap               (AttrMap, attrMap, attrName)
import qualified Brick.BChan
import           Brick.Main                  (App (App), appAttrMap,
                                              appChooseCursor, appDraw,
                                              appHandleEvent, appStartEvent,
                                              continue, customMain, halt,
                                              showFirstCursor, suspendAndResume)
import           Brick.Types                 (BrickEvent (AppEvent, VtyEvent),
                                              EventM, Next, Widget)
import           Brick.Util                  (bg, fg, on)
import qualified Control.Concurrent
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar)
import qualified Control.Monad
import qualified Control.Monad.STM
import           Graphics.Vty                as V
import           Lens.Micro                  ((^.))
import           System.Random               as R

import qualified Actions
import           Init                        (mkInitState)
import qualified Magic
import           Types
import qualified UI

data Tick = Tick

-- @returns the next GameState (wrapped in a Brick EventM).
appEvent :: GameState -> BrickEvent () Tick -> EventM () (Next GameState)
appEvent s (AppEvent Tick)           = continue $ Actions.tick s
appEvent s (VtyEvent (V.EvKey k [])) = appEventKey s k
appEvent s _                         = continue $ UI.redraw s

-- Helper function for appEvent, which only processes characters.
appEventKey :: GameState -> Key -> EventM () (Next GameState)
appEventKey s k
  | k == V.KEsc   || k == V.KChar 'q'  = halt s
  | k == V.KUp    || k == V.KChar 'w'  = continue $ UI.redraw $ Actions.move N s
  | k == V.KDown  || k == V.KChar 's'  = continue $ UI.redraw $ Actions.move S s
  | k == V.KLeft  || k == V.KChar 'a'  = continue $ UI.redraw $ Actions.move W s
  | k == V.KRight || k == V.KChar 'd'  = continue $ UI.redraw $ Actions.move E s
  | k == V.KChar ' '                   = continue $ UI.redraw $ Actions.spin s
  | k == V.KChar '1'                   = if s ^.over
                                           then suspendAndResume
                                                $ UI.redraw <$> mkInitState Easy
                                           else continue (UI.redraw s)
  | k == V.KChar '2'                   = if s ^.over
                                           then suspendAndResume
                                                $ UI.redraw <$> mkInitState Mid
                                           else continue (UI.redraw s)
  | k == V.KChar '3'                   = if s ^.over
                                           then suspendAndResume
                                                $ UI.redraw <$> mkInitState Hard
                                           else continue (UI.redraw s)
  | otherwise                          = continue $ UI.redraw s

-- @return initialized tick thread at getFrameRate (encapsulated in delayVar)
controlThread :: TVar Int -> Brick.BChan.BChan Tick -> IO ()
controlThread delay chan = Control.Monad.forever $ do
  Brick.BChan.writeBChan chan Tick
  ms <- Control.Monad.STM.atomically $ readTVar delay
  Control.Concurrent.threadDelay ms

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

  chan     <- Brick.BChan.newBChan Magic.getBChanQueueLength
  delayVar <- Control.Monad.STM.atomically $ newTVar Magic.getFrameRate
  Control.Monad.void $ Control.Concurrent.forkIO $ controlThread delayVar chan

  init_state <- UI.redraw <$> mkInitState Easy
  Control.Monad.void $ customMain init_vty (Just chan) init_app init_state
