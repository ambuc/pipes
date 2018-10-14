module Compute
    ( recomputeState
    , incrementTime
    ) where

import           Lens.Micro     ((%~), (&), (.~), (^.), (^?!))
import           Lens.Micro.GHC (each, ix)

import           Types
import           Util

incrementTime :: GameState -> GameState
incrementTime gs = gs & time %~ succ


-- @return the given GameState, with all DisplayTiles re-rendered as a
--         function of their respective Squares.
recomputeDisplays :: GameState -> GameState
recomputeDisplays gs = gs & board . each %~ makeDisplayTile

-- @return the given GameState, with all _visited and _flowstate fields updated
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
    trickleFrom n dir (h,w) b = if n >= 100 || b ^?! ix (h,w) . visited
                                   then b
                                   else exploreNeighbors n (h,w)
                                        $ ix (h,w) . visited   .~ True
                                        $ ix (h,w) . connected .~ True
                                        $ ix (h,w) . flow .~ Just (Flow In Out Out Out n)
                                        $ b
      where
        -- @return the given Board, with all adjacent squares explored.
        exploreNeighbors :: Int -> (Int, Int) -> Board -> Board
        exploreNeighbors n loc b = exploreDir N n loc
                                 $ exploreDir E n loc
                                 $ exploreDir S n loc
                                 $ exploreDir W n loc
                                 b
          where
            -- @return the given Board, with the adjacent square in the
            --         given direction tested for connectivity / colored in.
            exploreDir :: Dir -> Int -> (Int, Int) -> Board -> Board
            exploreDir dir n loc b = if canReach b loc dir
                                        then trickleFrom (succ n) dir (adj dir loc) b
                                        else b


-- @return the given GameState, with the location of the cursor set.
recomputeCursor :: GameState -> GameState
recomputeCursor gs = gs & board . ix (gs ^. cursor) . hascursor .~ True

-- TODO(ambuc): This could be a %~ resetSquare for one traversal.
-- @return the given GameState, with all rendering artifacts cleared.
resetAll :: GameState -> GameState
resetAll gs = gs & board . each . hascursor   .~ False
                 & board . each . connected   .~ False
                 & board . each . visited     .~ False
                 & board . each . displaytile .~ DisplayTile Z Z Z Z
                 & board . each . flow        .~ Nothing

-- @return the given GameState, with all visual elements re-rendered as a
--         function of the current cursor / tile configuration.
recomputeState :: GameState -> GameState
recomputeState = recomputeDisplays
               . recomputeCursor
               . recomputeFlow
               . resetAll

