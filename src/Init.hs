module Init where

import           Control.Monad (replicateM)
import           Data.Array    (Array, array, (!))
import qualified System.Random as Random

import           Lens.Micro    ((%~), (&), (.~), (^.))

import           Magic         (getBoardHeight, getBoardWidth)
import           Types
import           Util          (rotate)

-- @return a Tile of the given Shape.
mkTile :: Shape -> Tile -- N E W S
mkTile Null  = (False, False, False, False)  -- ' '
mkTile Line  = (False,  True,  True, False)  -- '─'
mkTile Bend  = ( True,  True, False, False)  -- '└'
mkTile Tee   = ( True,  True, False,  True)  -- '├'
mkTile Cross = ( True,  True,  True,  True)  -- '┼'
mkTile Nub   = ( True, False, False, False)  -- '╵'

-- @return an empty Square.
mkEmptySquare :: Square
mkEmptySquare = Square {        _tile = (False, False, False, False)
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
  -- arrays list numerically, i.e. first item of tuple constant, second item
  -- varying. if we want assocs to list over, we need to store (row, col)
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
      random_enum <- Random.randomRIO (3,4) -- (0, fromEnum (maxBound :: Shape))
      random_rot  <- Random.randomRIO (0, 3)
      let tile = mkTile $ toEnum random_enum
      let rotated_tile = iterate rotate tile !! random_rot
      return rotated_tile


--type WallSet = Array (Int, Int) Bool
--
--mkEmptyWallSet :: (Int, Int) -> WallSet
--mkEmptyWallSet (h,w) =  array ( (  0,  0)
--                              , (h-1,w-1)
--                              ) $ zip [ (y,x) | x <- [0..w-1], y <- [0..h-1] ]
--                                      (repeat False)
--
--mkWallSet :: (Int, Int) -> WallSet -> IO WallSet
--mkWallSet (h,w) ws
--  | h <= 2 || w <= 2 = return ws
--  | otherwise        = return ws
--
--squareFromWalls :: (Int, Int) -> WallSet -> Square
--squareFromWalls (h,w) ws = mkEmptySquare { _tile = (northWall, eastWall
--                                                   , westWall, southWall) }
--  where
--   northWall = ws ! ((2*h+1) - 1, (2*w+1) + 0)
--   southWall = ws ! ((2*h+1) + 1, (2*w+1) + 0)
--   eastWall  = ws ! ((2*h+1) + 0, (2*w+1) + 1)
--   westWall  = ws ! ((2*h+1) + 0, (2*w+1) - 1)
--
--wallSetToBoard :: (Int, Int) -> WallSet -> Board
--wallSetToBoard (h,w) ws = array ( (0,0) , (h-1,w-1) )
--                                [ ( (y,x)
--                                  , squareFromWalls (y,x) ws
--                                  )
--                                | x <- [0..w-1], y <- [0..h-1]
--                                ]
--
--
--mkRandomMaze :: (Int, Int) -> IO Board -- (h, w)
--mkRandomMaze (h,w) = do
--  wall_set <- mkWallSet (2*h+1,2*w+1)
--       $ mkEmptyWallSet (2*h+1,2*w+1)
--  return $ wallSetToBoard (h,w) wall_set

-- @return the initial, shuffled GameState.
mkInitState :: IO GameState
mkInitState = do
  init_border <- mkRandomBorder
  init_board  <- mkRandomBoard -- mkRandomMaze (getBoardHeight, getBoardWidth)
  let init_cursor = (\(h,w) -> (h+1, w)) $ init_border ^. tapLocation
  return GameState { _border = init_border
                   ,  _board = init_board
                   , _cursor = init_cursor
                   ,   _time = 0
                   }
