{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Data.Array    (Array)
import           Data.List     (elemIndex)

import           Lens.Micro.TH (makeLenses)

data Resource = SomeResource
  deriving (Eq, Ord)

data Wise   = CW | CCW

data Dir    = N | E | W | S

data FlowDirection = In | Out

data Flow = Flow { _flowN :: FlowDirection
                 , _flowE :: FlowDirection
                 , _flowW :: FlowDirection
                 , _flowS :: FlowDirection
                 ,  _dist :: Int
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
                 , _tS :: Bool
                 , _tW :: Bool }
  deriving (Eq)

data DisplayTile = DisplayTile { _dtN :: Fill
                               , _dtE :: Fill
                               , _dtS :: Fill
                               , _dtW :: Fill }

instance Show DisplayTile where
  show (DisplayTile n e w s) = [corpus !! (27 * fromEnum s
                                          + 9 * fromEnum w
                                          + 3 * fromEnum e
                                          + 1 * fromEnum n)]

data Shape  = Blank | Line | Bend | Tee | Cross | Culdesac
  deriving (Bounded, Enum)


data Square = Square {        _tile :: Tile
                     , _displaytile :: DisplayTile
                     ,   _connected :: Bool
                     ,        _flow :: Maybe Flow
                     ,     _visited :: Bool
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
                           }

makeLenses ''Tile
makeLenses ''DisplayTile
makeLenses ''Square
makeLenses ''Border
makeLenses ''GameState
