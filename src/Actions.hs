module Actions where

import           Data.Array ((!), (//))

import           Lens.Micro ((%~), (&), (^.))

import           Types
import           Util

-- @return the argument GameState, with the cursor moved one unit in the
--         given direction.
move :: Dir -> GameState -> GameState
move dir gs = gs & cursor %~ adj dir

-- @return the argument GameState, with the Tile under the Cursor rotated in
--         the given rotational direction.
rotateCursor :: Wise -> GameState -> GameState
rotateCursor w gs = gs & board %~ rotateBoard w (gs ^. cursor)
  where
    rotateBoard :: Wise -> (Int, Int) -> Board -> Board
    rotateBoard w cursor b = b // [( cursor, rotateSquare w ( b ! cursor ))]
      where
        rotateSquare :: Wise -> Square -> Square
        rotateSquare w s = s & tile %~ rotate w


