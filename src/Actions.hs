module Actions where

import           Data.Array     ((!), (//))

import           Lens.Micro     ((%~), (&), (^.), (^?!))
import           Lens.Micro.GHC (each, ix)

import           Magic
import           Types
import           Util

-- @return the argument GameState, with the cursor moved one unit in the
--         given direction.
move :: Dir -> GameState -> GameState
move dir gs = gs & cursor %~ adj dir

-- @return the argument GameState, with the Tile under the Cursor rotated in
--         the given rotational direction.
rotateCursor :: GameState -> GameState
rotateCursor gs = gs & board . ix (gs ^. cursor) . tile %~ rotate


