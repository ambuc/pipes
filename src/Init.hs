module Init
   ( mkInitState
   ) where

import           Control.Monad (replicateM)
import           Data.Array    (Array, array, listArray, (!))
import           Lens.Micro    ((%~), (&), (.~), (^.))
import qualified System.Random as Random

import           Magic         (getBoardBounds, getBoardHeight, getBoardWidth)
import           Types
import           Util

mkInitState :: IO GameState
mkInitState = do
  init_border <- mkRandomBorder
  init_board <- mkRandomBoard
  let init_cursor = (\(h,w) -> (h+1, w)) $ init_border ^. tapLocation
  return GameState { _border = init_border
                   ,  _board = init_board
                   , _cursor = init_cursor
                   ,   _time = 0
                   ,   _over = False
                   }

--------------------------------------------------------------------------------

random_doubles :: Int -> IO [Double]
random_doubles n = do
  gen <- Random.getStdGen
  return $ take n $ Random.randomRs (0.0, 1.0) gen

randomWeights :: IO (Array (Int, Int) Double)
randomWeights = do
  let (w,h) = getBoardBounds
  wts <- random_doubles (w*h)
  return $ listArray ((0,0), (w-1,h-1)) $ wts



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
    (h,w) = getBoardBounds

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
      random_enum <- Random.randomRIO (1, fromEnum (maxBound :: Shape))
      random_rot  <- Random.randomRIO (0, 3)
      let tile = shapeToTile $ toEnum random_enum
      let rotated_tile = iterate rotate tile !! random_rot
      return rotated_tile


