{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Data.Array    (Array)
import           Data.List     (elemIndex)

import           Lens.Micro.TH (makeLenses)

data Tick = Tick

data Dir    = N | E | W | S

data FlowDir = In | Out deriving (Eq)

corpus :: String
-- N      012012012012012012012012012012012012012012012012012012012012012012012012012012012
-- E      0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--
-- S      0--------1--------2--------0--------1--------2--------0--------1--------2--------
-- W      0--------------------------1--------------------------2--------------------------
corpus = " ╵╹╶└┖╺┕┗╷│╿┌├┞┍┝┡╻╽┃┎┟┠┏┢┣╴┘┚─┴┸╼┶┺┐┤┦┬┼╀┮┾╄┒┧┨┰╁╂┲╆╊╸┙┛╾┵┹━┷┻┑┥┩┭┽╃┯┿╇┓┪┫┱╅╉┳╈╋"

data Fill = Z | A | B
  deriving (Bounded, Enum) -- zilch, average, bold

 -- N E W S
type Tile        = (Bool, Bool, Bool, Bool)
type DisplayTile = (Fill, Fill, Fill, Fill)
type Flow        = (FlowDir, FlowDir, FlowDir, FlowDir )

data Shape  = Blank | Line | Bend | Tee | Cross | Culdesac
  deriving (Bounded, Enum)

data Square = Square {        _tile :: Tile
                     ,        _flow :: Maybe Flow
                     ,    _distance :: Maybe Int
                     ,   _hascursor :: Bool
                     }

type Board = Array (Int, Int) Square

data Border = Border {   _tapLocation :: (Int,Int)
                     , _drainLocation :: (Int,Int)
                     }

data GameState = GameState {  _board :: Board
                           , _border :: Border
                           , _cursor :: (Int, Int)
                           ,   _time :: Int
                           }

makeLenses ''Square
makeLenses ''Border
makeLenses ''GameState
