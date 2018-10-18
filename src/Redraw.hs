module Redraw
    ( redraw
    ) where

import           Data.Maybe     (isJust)
import           Lens.Micro     ((%~), (&), (.~), (^.), (^?!))
import           Lens.Micro.GHC (each, ix)

import           Init           (mkEmptySquare)
import           Magic
import           Types
import           Util

-- @return the given GameState, with all _flowstate fields updated
--         to reflect the new connectivity graph.
recomputeFlow :: GameState -> GameState
recomputeFlow gs = gs & board %~ (if entry_is_connected
                                    then trickleFrom 1 N entry_xy
                                    else id)
  where
    (tN, _, _, _) = gs ^. board ^?! ix entry_xy . tile
    entry_is_connected = tN
    entry_xy = (\(h,w) -> (h+1, w)) $ gs ^. border . tapLocation

    -- @return the given Board, with all Squares adjacent to the the Square
    --         at the input coordinates explored / colored in.
    trickleFrom :: Int -> Dir -> (Int, Int) -> Board -> Board
    trickleFrom dist dir (h,w) b
      | shouldVisit = b
      | otherwise   = exploreNeighbors dist (h,w)
                      $ ix (h,w) . flow %~ addFlowFrom (inv dir)
                      $ ix (h,w) . distance .~ Just dist
                      $ b
      where
        shouldVisit = (dist >= getMaxExploreDist)
                   || ( isJust (b ^?! ix (h,w) . distance)
                             && b ^?! ix (h,w) . distance <= Just dist)

    -- @return the given Board, with all adjacent squares explored.
    exploreNeighbors :: Int -> (Int, Int) -> Board -> Board
    exploreNeighbors dist loc b = exploreDir N dist loc
                                $ exploreDir E dist loc
                                $ exploreDir S dist loc
                                $ exploreDir W dist loc
                                b

    -- @return the given Board, with the adjacent square in the
    --         given direction tested for connectivity / colored in.
    exploreDir :: Dir -> Int -> (Int, Int) -> Board -> Board
    exploreDir dir dist loc b
      | canReach b loc dir = trickleFrom (succ dist) dir (adj dir loc) b
      | otherwise          = b

-- @return the given GameState, with the location of the cursor set.
recomputeCursor :: GameState -> GameState
recomputeCursor gs = gs & board . ix (gs ^. cursor) . hascursor .~ True

-- @return the given GameState, with all rendering artifacts cleared.
resetAll :: GameState -> GameState
resetAll gs = gs & board . each %~ resetSquare
  where
    resetSquare :: Square -> Square
    resetSquare Square {_tile = t} = mkEmptySquare { _tile = t}

-- @return the given GameState, with all visual elements re-rendered as a
--         function of the current cursor / tile configuration.
redraw :: GameState -> GameState
redraw = recomputeCursor
       . recomputeFlow
       . resetAll

