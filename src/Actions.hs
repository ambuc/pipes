module Actions where

import           Data.Array ((!), (//))

import           Lens.Micro (ix, (%~), (&), (^.))

import           Init       (getBoardHeight, getBoardWidth)
import           Types

move :: Dir -> GameState -> GameState
move N gs = gs & cursor %~ (\(h,w) -> (max 0 (h-1), w))
move S gs = gs & cursor %~ (\(h,w) -> (min (getBoardHeight-1) (h+1), w))
move W gs = gs & cursor %~ (\(h,w) -> (h, max 0 (w-1)))
move E gs = gs & cursor %~ (\(h,w) -> (h, min (getBoardWidth-1) (w+1)))

rotateSquare :: Wise -> Square -> Square
rotateSquare w s = s & tile %~ rotate w

rotateBoard :: Wise -> (Int, Int) -> Board -> Board
rotateBoard w cursor b = b // [( cursor, rotateSquare w ( b ! cursor ))]

rotateCursor :: Wise -> GameState -> GameState
rotateCursor w gs = gs & board %~ rotateBoard w (gs ^. cursor)


