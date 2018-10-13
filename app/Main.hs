module Main where

import           Brick.AttrMap (AttrMap, attrMap, attrName)
import           Brick.Main    (App (App), appAttrMap, appChooseCursor, appDraw,
                                appHandleEvent, appStartEvent, continue,
                                customMain, halt, showFirstCursor)
import           Brick.Types   (BrickEvent (VtyEvent), EventM, Next, Widget)
import           Control.Monad (void)
import           Graphics.Vty  as Vty
import           System.Random as R

import           Init
import           Lib           ()
import           Types         (GameState (..), Resource (..))
import           UI


appEvent :: GameState
         -> BrickEvent Resource e
         -> EventM Resource (Next GameState)
appEvent s (VtyEvent e) = case e of
  Vty.EvKey Vty.KEsc        [] -> halt s
  Vty.EvKey (Vty.KChar 'q') [] -> halt s
  _                            -> continue s
appEvent s _                    = continue s

aMap :: AttrMap
aMap = attrMap Vty.defAttr
     [ ( attrName "bold", withStyle defAttr bold)
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
  let init_vty = Vty.mkVty =<< Vty.standardIOConfig
  let init_channel = Nothing
  let init_app = mkApp
  init_state <- Init.mkState
  void $ customMain init_vty init_channel init_app init_state
