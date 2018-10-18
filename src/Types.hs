{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Data.Array    (Array)

import           Lens.Micro.TH (makeLenses)

data Tick        = Tick
data Dir         = N | E | W | S
data FlowDir     = In | Out deriving (Eq, Show)
data Fill        = Z | A | B deriving (Bounded, Enum) -- zilch, average, bold
type Tile        = (Bool, Bool, Bool, Bool) -- N E W S
type DisplayTile = (Fill, Fill, Fill, Fill)
type Flow        = (FlowDir, FlowDir, FlowDir, FlowDir)
data Shape       = Null | Line | Bend | Tee | Cross | Nub
  deriving (Bounded, Enum)
data Square      = Square {        _tile :: Tile
                          ,        _flow :: Maybe Flow
                          ,    _distance :: Maybe Int
                          ,   _hascursor :: Bool
                          } deriving (Show)
type Board       = Array (Int, Int) Square
data Border      = Border {   _tapLocation :: (Int,Int)
                          , _drainLocation :: (Int,Int)
                          }
data GameState   = GameState {  _board :: Board
                             , _border :: Border
                             , _cursor :: (Int, Int)
                             ,   _time :: Int
                             }

makeLenses ''Square
makeLenses ''Border
makeLenses ''GameState
