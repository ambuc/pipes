module Actions
    ( move
    , spin
    , tick
    ) where

import           Lens.Micro     ((%~), (&), (^.))
import           Lens.Micro.GHC (ix)

import           Types
import           Util           (cursorAdj, rotate)

-- @return the argument GameState, with the cursor moved one unit in the given
--         direction.
move :: Dir -> GameState -> GameState
move dir gs = if gs ^. over
                then gs
                else gs & cursor %~ cursorAdj dir

-- @return the argument GameState, with the Tile under the Cursor rotated in the
--         given rotational direction.
spin :: GameState -> GameState
spin gs = if gs ^. over
            then gs
            else gs & board . ix (gs ^. cursor) . tile %~ rotate

-- @return the given GameState, ticked forward one time unit.
tick :: GameState -> GameState
tick gs = gs & time %~ succ

