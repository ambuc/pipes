{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Data.Array    (Array)
import           Data.List     (elemIndex)

import           Lens.Micro.TH (makeLenses)



data Resource = Up | Down | Left | Right deriving (Eq, Show, Ord)

corpus :: String
-- N      012012012012012012012012012012012012012012012012012012012012012012012012012012012
-- E      0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--
-- S      0--------1--------2--------0--------1--------2--------0--------1--------2--------
-- W      0--------------------------1--------------------------2--------------------------
corpus = " ╵╹╶└┖╺┕┗╷│╿┌├┞┍┝┡╻╽┃┎┟┠┏┢┣╴┘┚─┴┸╼┶┺┐┤┦┬┼╀┮┾╄┒┧┨┰╁╂┲╆╊╸┙┛╾┵┹━┷┻┑┥┩┭┽╃┯┿╇┓┪┫┱╅╉┳╈╋"

data Fill = Z | A | B deriving (Show, Bounded, Ord, Eq, Enum) -- zilch, average, bold

data Tile = Tile { _north :: Fill
                 ,  _east :: Fill
                 , _south :: Fill
                 ,  _west :: Fill } deriving (Eq)
instance Show Tile where
  show (Tile n e w s) = [corpus !! (27 * fromEnum s
                                   + 9 * fromEnum w
                                   + 3 * fromEnum e
                                   + 1 * fromEnum n)]
data Wise = CW | CCW

rotate :: Wise -> Tile -> Tile
rotate CW (Tile a b c d)  = Tile d a b c
rotate CCW (Tile a b c d) = Tile b c d a

data Shape = Blank | Line | Bend | Tee | Cross | Culdesac deriving (Show, Bounded, Eq, Enum)

-- a ─ │      <=> ━ ┃      (and intermediates)
-- b  ┌ ┐ └ ┘ <=>  ┏ ┓ ┗ ┛
-- c  ├ ┬ ┤ ┴ <=>  ┣ ┳ ┫ ┻
-- d ┼        <=> ╋
-- e ╴ ╵ ╶ ╷  <=> ╸ ╹ ╺ ╻

data Dir    = N | E | W | S deriving (Show, Eq, Enum)
data Flow   = NotFlowing | FlowingIn | FlowingOut deriving (Show, Eq)
data Square = Square {          _tile :: Tile
                     ,     _flowstate :: Flow       -- for animation
                     , _flowdirection :: Maybe Dir  -- for animation
                     } deriving (Eq)
instance Show Square where
  show Square {_tile = t} = show t

type Board = Array (Int, Int) Square

data Border = Border {   _tapLocation :: (Int,Int)
                     , _drainLocation :: (Int,Int)
                     } deriving (Show)

data GameState = GameState {  _board :: Board
                           , _border :: Border
                           , _cursor :: (Int, Int)
                           } deriving (Show)


makeLenses ''Tile
makeLenses ''Square
makeLenses ''Border
makeLenses ''GameState
