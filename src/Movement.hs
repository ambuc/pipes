module Movement where

import           Data.Array ((!), (//))

import           Lens.Micro (ix, (%~), (&), (^.))

import           Init       (getBoardHeight, getBoardWidth)
import           Types

move :: Dir -> GameState -> GameState
move N gs = gs & cursor %~ (\(h,w) -> (max 0 (h-1), w))
move S gs = gs & cursor %~ (\(h,w) -> (min (getBoardHeight-1) (h+1), w))
move W gs = gs & cursor %~ (\(h,w) -> (h, max 0 (w-1)))
move E gs = gs & cursor %~ (\(h,w) -> (h, min (getBoardWidth-1) (w+1)))

rotateSquare :: Square -> Square
rotateSquare s = s & tile %~ rotateCW

rotateBoard :: (Int, Int) -> Board -> Board
rotateBoard cursor b = b // [( cursor, rotateSquare ( b ! cursor ))]

rotateCursor :: GameState -> GameState
rotateCursor gs = gs & board %~ rotateBoard (gs ^. cursor)

