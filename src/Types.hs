{-# LANGUAGE TemplateHaskell #-}

module Types where

import qualified Data.Array
import qualified Lens.Micro.TH

data Difficulty  = Easy | Mid | Hard deriving (Eq)
data Dir         = N | E | W | S
data FlowDir     = In | Out deriving (Eq, Show)
type Tile        = (Bool, Bool, Bool, Bool) -- N E W S
type Flow        = (FlowDir, FlowDir, FlowDir, FlowDir) -- N E W S
data Square      = Square {        _tile :: Tile
                          ,        _flow :: Maybe Flow
                          ,    _distance :: Maybe Int
                          ,   _hascursor :: Bool
                          ,    _isborder :: Bool
                          } deriving (Show)
type Board       = Data.Array.Array (Int, Int) Square -- (Y, X)
data GameState   = GameState {      _board :: Board
                             ,        _tap :: (Int, Int) -- (Y, X)
                             ,      _drain :: (Int, Int) -- (Y, X)
                             ,     _cursor :: (Int, Int) -- (Y, X)
                             ,       _time :: Int -- tick time
                             ,      _timer :: Int -- game time
                             ,       _over :: Bool
                             ,    _maxdist :: Int
                             ,      _moves :: Int
                             , _difficulty :: Difficulty
                             }

Lens.Micro.TH.makeLenses ''Square
Lens.Micro.TH.makeLenses ''GameState
