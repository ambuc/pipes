module Types where

import           Data.Array (Array)
import           Data.List  (elemIndex)

data Resource = SomeResource deriving (Eq, Show, Ord)

corpus :: String
-- N      012012012012012012012012012012012012012012012012012012012012012012012012012012012
-- E      0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--
-- S      0--------1--------2--------0--------1--------2--------0--------1--------2--------
-- W      0--------------------------1--------------------------2--------------------------
corpus = " ╵╹╶└┖╺┕┗╷│╿┌├┞┍┝┡╻╽┃┎┟┠┏┢┣╴┘┚─┴┸╼┶┺┐┤┦┬┼╀┮┾╄┒┧┨┰╁╂┲╆╊╸┙┛╾┵┹━┷┻┑┥┩┭┽╃┯┿╇┓┪┫┱╅╉┳╈╋"

data Fill = Z | A | B deriving (Show, Bounded, Ord, Eq, Enum) -- zilch, average, bold

data Tile = Tile { north :: Fill
                 ,  east :: Fill
                 , south :: Fill
                 ,  west :: Fill } deriving (Eq)
instance Show Tile where
  show (Tile n e w s) = [corpus !! (27 * fromEnum s
                                   + 9 * fromEnum w
                                   + 3 * fromEnum e
                                   + 1 * fromEnum n)]
rotateCW :: Tile -> Tile
rotateCW (Tile a b c d) = Tile d a b c

rotateCCW :: Tile -> Tile
rotateCCW (Tile a b c d) = Tile b c d a

data Shape = Line | Bend | Tee | Cross | Culdesac deriving (Show, Bounded, Eq, Enum)

-- a ─ │      <=> ━ ┃      (and intermediates)
-- b  ┌ ┐ └ ┘ <=>  ┏ ┓ ┗ ┛
-- c  ├ ┬ ┤ ┴ <=>  ┣ ┳ ┫ ┻
-- d ┼        <=> ╋
-- e ╴ ╵ ╶ ╷  <=> ╸ ╹ ╺ ╻

data Dir    = N | E | W | S deriving (Show, Eq, Enum)
data Flow   = NotFlowing | FlowingIn | FlowingOut deriving (Show, Eq)
data Square = Square {          tile :: Maybe Tile
                     ,     flowstate :: Flow       -- for animation
                     , flowdirection :: Maybe Dir  -- for animation
                     ,    isselected :: Bool       -- is the cursor over this square?
                     } deriving (Eq)
instance Show Square where
  show Square {tile = Just t , isselected = True } = "."
  show Square {tile = Just t , isselected = False} = show t
  show Square {tile = Nothing, isselected = True } = "░"
  show Square {tile = Nothing, isselected = False} = " "

type Board = Array (Int, Int) Square

data Border = Border {   tapLocation :: Int
                     , drainLocation :: Int
                     } deriving (Show)

data GameState = GameState {  board :: Board
                           , border :: Border
                           , cursor :: (Int, Int)
                           } deriving (Show)

