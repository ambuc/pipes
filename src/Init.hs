module Init where

import           Control.Monad (replicateM)
import           Data.Array    (array)
import qualified System.Random as Random

import           Lens.Micro    ((%~), (&), (.~), (^.))

import           Types
import           Util

-- @return a Tile of the given Shape.
mkTile :: Shape -> Tile -- N E W S
mkTile Blank    = Tile False False False False  -- ' '
mkTile Line     = Tile False True  True  False   -- '─'
mkTile Bend     = Tile True  True  False False  -- '└'
mkTile Tee      = Tile True  True  False True  -- '├'
mkTile Cross    = Tile True  True  True  True   -- '┼'
mkTile Culdesac = Tile True  False False False  -- '╵'

-- @return an empty Square.
mkEmptySquare :: Square
mkEmptySquare = Square {        _tile = nullTile
                       , _displaytile = DisplayTile Z Z Z Z
                       ,        _flow = Nothing
                       ,    _distance = Nothing
                       ,   _hascursor = False
                       }

-- @return a random Border object with tap/drain locations.
mkRandomBorder :: IO Border
mkRandomBorder = do
  n <- Random.randomRIO (0, getBoardWidth-1)
  m <- Random.randomRIO (0, getBoardWidth-1)
  return Border {   _tapLocation = (            -1, n)
                , _drainLocation = (getBoardHeight, m)
                }

-- @return a shuffled Board.
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

    -- @return a random Square.
    mkRandomSquare :: IO Square
    mkRandomSquare = do
      tile <- mkRandomTile
      return $ mkEmptySquare { _tile = tile }
      where
        -- TODO(ambuc): This could have a tile distribution.
        -- @return a random Tile at a random rotation.
        mkRandomTile :: IO Tile
        mkRandomTile = do
          random_enum <- Random.randomRIO (0, fromEnum (maxBound :: Shape))
          random_rot  <- Random.randomRIO (0, 3)
          let tile = mkTile $ toEnum random_enum
          let rotated_tile = iterate (rotate CW) tile !! random_rot
          return rotated_tile

-- @return the initial, shuffled GameState.
mkState :: IO GameState
mkState = do
  init_border <- mkRandomBorder
  init_board  <- mkRandomBoard
  let init_cursor = (\(h,w) -> (h+1, w)) $ init_border ^. tapLocation
  return GameState { _border = init_border
                   ,  _board = init_board
                   , _cursor = init_cursor
                   ,   _time = 0
                   }
