module Init where

import           Control.Monad (replicateM)
import           Data.Array    (array)
import qualified System.Random as Random

import           Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

mkTile :: Shape -> Tile
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
  let rotated_tile = iterate rotateCW tile !! random_rot
  return rotated_tile

mkEmptySquare :: Square
mkEmptySquare = Square {          tile = Nothing
                       ,     flowstate = NotFlowing
                       , flowdirection = Nothing
                       ,    isselected = False
                       }

mkRandomSquare :: IO Square
mkRandomSquare = do
  tile <- mkRandomTile
  return $ mkEmptySquare { tile = Just tile }

getBoardWidth = 20
getBoardHeight = 10

mkRandomBoard :: IO Board
mkRandomBoard = do
  random_squares <- replicateM (w * h) mkRandomSquare
  return $ array ( (  0,   0)
                 , (w-1, h-1) -- w x h
                 ) $ zip [ (i,j) | i <- [0..w-1] , j <- [0..h-1]]
                         random_squares
  where
    h = getBoardHeight
    w = getBoardWidth

mkRandomBorder :: IO Border
mkRandomBorder = do
  n <- Random.randomRIO (0, getBoardWidth-1)
  m <- Random.randomRIO (0, getBoardWidth-1)
  return Border {   tapLocation = n
                , drainLocation = m
                }

mkState :: IO GameState
mkState = do
  random_border <- mkRandomBorder
  random_board <- mkRandomBoard
  let init_cursor = (div getBoardWidth 2, div getBoardHeight 2) -- (x,y)
  return GameState { border = random_border
                   ,  board = random_board
                   , cursor = init_cursor
                   }
