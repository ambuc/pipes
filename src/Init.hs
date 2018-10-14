module Init where

import           Control.Monad (replicateM)
import           Data.Array    (array)
import qualified System.Random as Random

import           Lens.Micro    ((%~), (&), (.~))

import           Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

mkTile :: Shape -> Tile
mkTile Blank    = Tile Z Z Z Z -- ' '
mkTile Line     = Tile Z A Z A -- '─'
mkTile Bend     = Tile A A Z Z -- '└'
mkTile Tee      = Tile A A A Z -- '├'
mkTile Cross    = Tile A A A A -- '┼'
mkTile Culdesac = Tile A Z Z Z -- '╵'

-- TODO(ambuc): This could have a tile distribution.
mkRandomTile :: IO Tile
mkRandomTile = do
  random_enum <- Random.randomRIO (0, fromEnum (maxBound :: Shape))
  random_rot <- Random.randomRIO (0, 3)
  let tile = mkTile $ toEnum random_enum
  let rotated_tile = iterate (rotate CW) tile !! random_rot
  return rotated_tile

mkEmptySquare :: Square
mkEmptySquare = Square {          _tile = Tile Z Z Z Z
                       ,     _flowstate = NotFlowing
                       , _flowdirection = Nothing
                       }

mkRandomSquare :: IO Square
mkRandomSquare = do
  tile <- mkRandomTile
  return $ mkEmptySquare { _tile = tile }

getBoardWidth = 20
getBoardHeight = 10

mkRandomBoard :: IO Board
mkRandomBoard = do
  random_squares <- replicateM (w * h) mkRandomSquare
  -- arrays list numerically, i.e. first item of tuple constant, second item varying
  -- if we want assocs to list over, we need to store (row, col)
  return $ array ( (  0,   0)
                 , (h-1, w-1) -- w x h
                 ) $ zip [ (y,x) | x <- [0..w-1] , y <- [0..h-1]]
                         random_squares
  where
    h = getBoardHeight
    w = getBoardWidth

mkRandomBorder :: IO Border
mkRandomBorder = do
  n <- Random.randomRIO (0, getBoardWidth-1)
  m <- Random.randomRIO (0, getBoardWidth-1)
  return Border {   _tapLocation = n
                , _drainLocation = m
                }

mkState :: IO GameState
mkState = do
  random_border <- mkRandomBorder
  random_board <- mkRandomBoard
  let init_cursor = (div getBoardHeight 2, div getBoardWidth 2) -- (h,w)
  return GameState { _border = random_border
                   ,  _board = random_board
                   , _cursor = init_cursor
                   }
