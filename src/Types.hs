{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Data.Array    (Array)
import           Data.List     (elemIndex)

import           Lens.Micro.TH (makeLenses)

data Tick = Tick

data Wise   = CW | CCW

data Dir    = N | E | W | S

data FlowDirection = In | Out

data Flow = Flow { _flowN :: FlowDirection
                 , _flowE :: FlowDirection
                 , _flowW :: FlowDirection
                 , _flowS :: FlowDirection
                 }

corpus :: String
-- N      012012012012012012012012012012012012012012012012012012012012012012012012012012012
-- E      0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--
-- S      0--------1--------2--------0--------1--------2--------0--------1--------2--------
-- W      0--------------------------1--------------------------2--------------------------
corpus = " ╵╹╶└┖╺┕┗╷│╿┌├┞┍┝┡╻╽┃┎┟┠┏┢┣╴┘┚─┴┸╼┶┺┐┤┦┬┼╀┮┾╄┒┧┨┰╁╂┲╆╊╸┙┛╾┵┹━┷┻┑┥┩┭┽╃┯┿╇┓┪┫┱╅╉┳╈╋"

data Fill = Z | A | B
  deriving (Bounded, Enum) -- zilch, average, bold

data Tile = Tile { _tN :: Bool
                 , _tE :: Bool
                 , _tW :: Bool
                 , _tS :: Bool
                 }
  deriving (Eq, Show)

data DisplayTile = DisplayTile { _dtN :: Fill
                               , _dtE :: Fill
                               , _dtW :: Fill
                               , _dtS :: Fill
                               }

instance Show DisplayTile where
  show (DisplayTile n e w s) = [corpus !! (27 * fromEnum w
                                          + 9 * fromEnum s
                                          + 3 * fromEnum e
                                          + 1 * fromEnum n)]

data Shape  = Blank | Line | Bend | Tee | Cross | Culdesac
  deriving (Bounded, Enum)


data Square = Square {        _tile :: Tile
                     , _displaytile :: DisplayTile
                     ,        _flow :: Maybe Flow
                     ,    _distance :: Maybe Int
                     ,   _hascursor :: Bool
                     }

instance Show Square where
  show Square {_displaytile = dt} = show dt

type Board = Array (Int, Int) Square

data Border = Border {   _tapLocation :: (Int,Int)
                     , _drainLocation :: (Int,Int)
                     }

data GameState = GameState {  _board :: Board
                           , _border :: Border
                           , _cursor :: (Int, Int)
                           ,   _time :: Int
                           }

makeLenses ''Flow
makeLenses ''Tile
makeLenses ''DisplayTile
makeLenses ''Square
makeLenses ''Border
makeLenses ''GameState
