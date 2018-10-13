module Main where

import           Brick.AttrMap              (AttrMap, attrMap, attrName)
import           Brick.Main                 (App (App), appAttrMap,
                                             appChooseCursor, appDraw,
                                             appHandleEvent, appStartEvent,
                                             continue, customMain, halt,
                                             showFirstCursor)
import           Brick.Types                (BrickEvent (VtyEvent), EventM,
                                             Next, Widget)
import           Brick.Widgets.Border       (borderWithLabel, vBorder)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center       (center)
import           Brick.Widgets.Core         (str, withBorderStyle, (<+>))
import           Control.Monad              (void)
import           Graphics.Vty               as Vty (Event (EvKey),
                                                    Key (KChar, KEsc), bold,
                                                    defAttr, mkVty,
                                                    standardIOConfig, withStyle)
import qualified System.Random              as R (StdGen, newStdGen, next)
import           Text.Printf                (printf)

import           Lib                        ()

data Resource = SomeResource deriving (Eq, Show, Ord)
data GameState = GameState {} deriving (Show)

drawUI :: GameState -> [Widget Resource]
drawUI _ = [ withBorderStyle unicode
           $ borderWithLabel (str "Hello!")
           $ center (str "Left") <+> vBorder <+> center (str "Right")
           ]

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
mkApp = App { appDraw         = drawUI          -- s -> [Widget n]
            , appChooseCursor = showFirstCursor -- s -> [CursorLocation n] -> Maybe (CursorLocation n)
            , appHandleEvent  = appEvent        -- s -> BrickEvent n e -> EventM n (Next s)
            , appStartEvent   = return          -- s -> EventM n s
            , appAttrMap      = const aMap      -- s -> AttrMap
            }

mkInitState :: R.StdGen -> GameState
mkInitState _ = GameState {}

main :: IO ()
main = do
  let init_vty = Vty.mkVty =<< Vty.standardIOConfig
  let init_channel = Nothing
  let init_app = mkApp
  init_state <- mkInitState <$> R.newStdGen
  void $ customMain init_vty init_channel init_app init_state
