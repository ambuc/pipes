module Compute
    ( recomputeState
    , incrementTime
    ) where

import           Data.Maybe     (isJust)
import           Lens.Micro     ((%~), (&), (.~), (^.), (^?!))
import           Lens.Micro.GHC (each, ix)

import           Types
import           Util

incrementTime :: GameState -> GameState
incrementTime gs = recomputeDisplays
                 $ gs & time %~ succ

-- @return the given GameState, with all DisplayTiles re-rendered as a
--         function of their respective Squares.
recomputeDisplays :: GameState -> GameState
recomputeDisplays gs = gs & board . each %~ setDisplayTile (gs ^. time)

-- @return the given GameState, with all _flowstate fields updated
--         to reflect the new connectivity graph.
recomputeFlow :: GameState -> GameState
recomputeFlow gs = gs & board %~ (if entry_is_connected
                                    then trickleFrom 0 N entry_xy
                                    else id)
  where
    entry_is_connected = gs ^. board ^?! ix entry_xy . tile . tN
    entry_xy = (\(h,w) -> (h+1, w)) $ gs ^. border . tapLocation

    -- @return the given Board, with all Squares adjacent to the the Square
    --         at the input coordinates explored / colored in.
    trickleFrom :: Int -> Dir -> (Int, Int) -> Board -> Board
    trickleFrom dist dir (h,w) b = if shouldVisit
                                     then b
                                     else exploreNeighbors dist (h,w)
                                          $ ix (h,w) . flow %~ addFlowFrom dir
                                          $ ix (h,w) . distance .~ Just dist
                                          $ b
      where
        shouldVisit = (dist >= 200)
                   || ( isJust (b ^?! ix (h,w) . distance)
                             && b ^?! ix (h,w) . distance <= Just dist
                      )
        -- @return the given Board, with all adjacent squares explored.
        exploreNeighbors :: Int -> (Int, Int) -> Board -> Board
        exploreNeighbors dist loc b = exploreDir N dist loc
                                    $ exploreDir E dist loc
                                    $ exploreDir S dist loc
                                    $ exploreDir W dist loc
                                    b
          where
            -- @return the given Board, with the adjacent square in the
            --         given direction tested for connectivity / colored in.
            exploreDir :: Dir -> Int -> (Int, Int) -> Board -> Board
            exploreDir dir dist loc b = if canReach b loc dir
                                        then trickleFrom (succ dist) dir (adj dir loc) b
                                        else b

-- @return the given GameState, with the location of the cursor set.
recomputeCursor :: GameState -> GameState
recomputeCursor gs = gs & board . ix (gs ^. cursor) . hascursor .~ True

-- TODO(ambuc): This could be a %~ resetSquare for one traversal.
-- @return the given GameState, with all rendering artifacts cleared.
resetAll :: GameState -> GameState
resetAll gs = gs & board . each . hascursor   .~ False
                 & board . each . distance    .~ Nothing
                 & board . each . displaytile .~ DisplayTile Z Z Z Z
                 & board . each . flow        .~ Nothing

-- @return the given GameState, with all visual elements re-rendered as a
--         function of the current cursor / tile configuration.
recomputeState :: GameState -> GameState
recomputeState = recomputeDisplays
               . recomputeCursor
               . recomputeFlow
               . resetAll

