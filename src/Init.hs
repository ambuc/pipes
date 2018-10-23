module Init
   ( mkInitState, mkRandomBoard'
   ) where

import           Control.Monad (replicateM)
import           Data.Array    (Array, array, listArray, (!))
import           Debug.Trace   (trace)
import           Lens.Micro    ((%~), (&), (.~), (^.))
import qualified System.Random as Random

import           Magic         (getBoardBounds, getBoardHeight, getBoardWidth)
import           Types
import           Util

mkInitState :: Difficulty -> IO GameState
mkInitState difficulty = do
  (init_board, tapYX, drainYX) <- mkRandomBoard' difficulty
  let init_cursor = (\(y,x) -> (y+1, x)) tapYX
  return GameState {      _board = init_board
                   ,        _tap = tapYX
                   ,      _drain = drainYX
                   ,     _cursor = init_cursor
                   ,       _time = 0
                   ,      _timer = 0
                   ,       _over = False
                   ,    _maxdist = 0
                   ,      _moves = 0
                   , _difficulty = Easy
                   }

--------------------------------------------------------------------------------

randomDoubles :: Int -> IO [Double]
randomDoubles n = do
  gen <- Random.getStdGen
  return $ take n $ Random.randomRs (0.0, 1.0) gen

randomWeights' :: IO (Array (Int, Int) Double)
randomWeights' = do
  let (w,h) = getBoardBounds
  wts <- randomDoubles (w*h)
  return $ listArray ((0,0), (w-1,h-1)) wts


-- @return a shuffled Board.
mkRandomBoard' :: Difficulty -> IO (Board, (Int, Int), (Int, Int))
mkRandomBoard' difficulty = do
  random_squares <- replicateM (w * h) $ mkRandomSquare' difficulty
  n              <- Random.randomRIO (0, getBoardWidth - 1)

  let tapYX@(_,tx)   = (-1, n)
  let drainYX@(_,dx) = (getBoardHeight, getBoardWidth - n - 1)
  let main_board     = zip [ (y,x) | x <- [0..w-1], y <- [0..h-1] ]
                           random_squares
  let top_border     = zip [ (y,x) | x <- [0..w-1], y <- [-1], x /= tx ]
                     $ repeat mkHorizontalBorderSquare
  let bottom_border  = zip [ (y,x) | x <- [0..w-1], y <- [h], x /= dx ]
                     $ repeat mkHorizontalBorderSquare
  let east_border    = zip [ (y,x) | x <- [w], y <- [0..h-1] ]
                     $ repeat mkVerticalBorderSquare
  let west_border    = zip [ (y,x) | x <- [-1], y <- [0..h-1] ]
                     $ repeat mkVerticalBorderSquare
  let tl_border      = [ ((-1,-1),    mkTLBorderSquare) ]
  let tr_border      = [ ((-1, w),    mkTRBorderSquare) ]
  let bl_border      = [ (( h,-1),    mkBLBorderSquare) ]
  let br_border      = [ (( h, w),    mkBRBorderSquare) ]
  let tap_border     = [ (  tapYX,   mkTapBorderSquare) ]
  let drain_border   = [ (drainYX, mkDrainBorderSquare) ]
  let main_board     = zip [ (y,x) | x <- [0..w-1] , y <- [0..h-1]]
                           random_squares
  let all_squares    = main_board
                       ++  top_border ++ bottom_border
                       ++ east_border ++   west_border
                       ++   tl_border ++     tr_border
                       ++   bl_border ++     br_border
                       ++  tap_border ++  drain_border
  let board          = array ( (-1, -1)
                             , ( h,  w) -- w x h
                             ) all_squares
  return (board, tapYX, drainYX)
  where
    (h,w) = getBoardBounds

-- @return a random Square.
mkRandomSquare' :: Difficulty -> IO Square
mkRandomSquare' difficulty = do
  tile <- mkRandomTile' difficulty
  return $ mkEmptySquare { _tile = tile }

mkRandomTile' :: Difficulty -> IO Tile
mkRandomTile' difficulty = do
  let base_tile = baseTileEnumFromDifficulty difficulty
  random_enum <- Random.randomRIO (base_tile, fromEnum (maxBound :: Shape))
  random_rot  <- Random.randomRIO (0, 3)
  let tile = shapeToTile $ toEnum random_enum
  let rotated_tile = iterate rotate tile !! random_rot
  return rotated_tile


