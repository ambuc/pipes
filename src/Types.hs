{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Data.Array    (Array)
import           Lens.Micro.TH (makeLenses)

data Difficulty  = Easy | Mid | Hard deriving (Eq)
data Dir         = N | E | W | S
data FlowDir     = In | Out deriving (Eq, Show)
type Tile        = (Bool, Bool, Bool, Bool) -- N E W S
type Flow        = (FlowDir, FlowDir, FlowDir, FlowDir)
data Square      = Square {        _tile :: Tile
                          ,        _flow :: Maybe Flow
                          ,    _distance :: Maybe Int
                          ,   _hascursor :: Bool
                          ,    _isborder :: Bool
                          } deriving (Show)
type Board       = Array (Int, Int) Square
data GameState   = GameState {      _board :: Board
                             ,        _tap :: (Int, Int)
                             ,      _drain :: (Int, Int)
                             ,     _cursor :: (Int, Int)
                             ,       _time :: Int -- tick time
                             ,      _timer :: Int -- game time
                             ,       _over :: Bool
                             ,    _maxdist :: Int
                             ,      _moves :: Int
                             , _difficulty :: Difficulty
                             }

makeLenses ''Square
-- makeLenses ''Border
makeLenses ''GameState
