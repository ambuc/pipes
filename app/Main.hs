module Main where

import           Brick.AttrMap (AttrMap, attrMap, attrName)
import           Brick.Main    (App (App), appAttrMap, appChooseCursor, appDraw,
                                appHandleEvent, appStartEvent, continue,
                                customMain, halt, showFirstCursor)
import           Brick.Types   (BrickEvent (VtyEvent), EventM, Next, Widget)
import           Brick.Util    (bg, fg, on)
import           Control.Monad (void)
import           Graphics.Vty  as Vty
import           System.Random as R

import           Init
import           Lib
import           Movement
import           Types         (Dir (..), GameState (..), Resource (..))
import           UI


appEvent :: GameState
         -> BrickEvent Resource e
         -> EventM Resource (Next GameState)
appEvent s (VtyEvent e) = case e of
  -- QUIT
  Vty.EvKey Vty.KEsc        [] -> halt s
  Vty.EvKey (Vty.KChar 'q') [] -> halt s
  -- UP
  Vty.EvKey (Vty.KChar 'w') [] -> continue $ move N s
  Vty.EvKey Vty.KUp         [] -> continue $ move N s
  -- DOWN
  Vty.EvKey (Vty.KChar 's') [] -> continue $ move S s
  Vty.EvKey Vty.KDown       [] -> continue $ move S s
  -- LEFT
  Vty.EvKey (Vty.KChar 'a') [] -> continue $ move W s
  Vty.EvKey Vty.KLeft       [] -> continue $ move W s
  -- RIGHT
  Vty.EvKey (Vty.KChar 'd') [] -> continue $ move E s
  Vty.EvKey Vty.KRight      [] -> continue $ move E s
  -- ROTATE
  Vty.EvKey (Vty.KChar ' ') [] -> continue $ rotateCursor s
  _                            -> continue s
appEvent s _                    = continue s

aMap :: AttrMap
aMap = attrMap Vty.defAttr
     [ (attrName "bold", withStyle defAttr bold)
     , (attrName "bg-blue", bg Vty.blue)
     , (attrName "fg-blue", fg Vty.blue)
     , (attrName "bg-red", bg Vty.red)
     , (attrName "fg-red", fg Vty.red)
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
