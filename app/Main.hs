module Main where

import           Brick.AttrMap (AttrMap, attrMap, attrName)
import           Brick.Main    (App (App), appAttrMap, appChooseCursor, appDraw,
                                appHandleEvent, appStartEvent, continue,
                                customMain, halt, showFirstCursor)
import           Brick.Types   (BrickEvent (VtyEvent), EventM, Next, Widget)
import           Brick.Util    (bg, fg, on)
import           Control.Monad (void)
import           Graphics.Vty  as V
import           System.Random as R

import           Actions
import           Compute
import           Init
import           Types
import           UI


appEvent :: GameState
         -> BrickEvent Resource e
         -> EventM Resource (Next GameState)
appEvent s (VtyEvent (V.EvKey V.KEsc []))        = halt s
appEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
appEvent s (VtyEvent (V.EvKey V.KUp []))         = continue $ recomputeState $ move N s
appEvent s (VtyEvent (V.EvKey V.KDown []))       = continue $ recomputeState $ move S s
appEvent s (VtyEvent (V.EvKey V.KLeft []))       = continue $ recomputeState $ move W s
appEvent s (VtyEvent (V.EvKey V.KRight []))      = continue $ recomputeState $ move E s
appEvent s (VtyEvent (V.EvKey (V.KChar 'w') [])) = continue $ recomputeState $ move N s
appEvent s (VtyEvent (V.EvKey (V.KChar 's') [])) = continue $ recomputeState $ move S s
appEvent s (VtyEvent (V.EvKey (V.KChar 'a') [])) = continue $ recomputeState $ move W s
appEvent s (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue $ recomputeState $ move E s
appEvent s (VtyEvent (V.EvKey (V.KChar ' ') [])) = continue $ recomputeState $ rotateCursor CW s
appEvent s _                                     = continue $ recomputeState s

aMap :: AttrMap
aMap = attrMap V.defAttr
     [ (attrName "bold", withStyle defAttr bold)
     , (attrName "bg-blue", bg V.blue)
     , (attrName "fg-blue", fg V.blue)
     , (attrName "bg-red", bg V.red)
     , (attrName "fg-red", fg V.red)
     ]

mkApp :: App GameState e Resource
mkApp = App { appDraw         = UI.draw         -- s -> [Widget n]
            , appChooseCursor = showFirstCursor -- s -> [CursorLocation n] -> Maybe (CursorLocation n)
            , appHandleEvent  = appEvent        -- s -> BrickEvent n e -> EventM n (Next s)
            , appStartEvent   = return          -- s -> EventM n s
            , appAttrMap      = const aMap      -- s -> AttrMap
            }

main :: IO ()
main = do
  let init_vty = V.mkVty =<< V.standardIOConfig
  let init_channel = Nothing
  let init_app = mkApp
  init_state <- recomputeState <$> Init.mkState
  void $ customMain init_vty init_channel init_app init_state
