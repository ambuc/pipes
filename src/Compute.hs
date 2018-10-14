module Compute
    ( recomputeState
    ) where

import           Data.Array     (assocs, elems, (!))
import           Text.Wrap

import           Lens.Micro     (at, (%~), (&), (.~), (^.), (^?!))
import           Lens.Micro.GHC (each, ix)

import           Types
import           Util

-- @return the given GameState, with all visual elements re-rendered as a
--         function of the current cursor / tile configuration.
recomputeState :: GameState -> GameState
recomputeState = recomputeDisplays
               . recomputeCursor
               . recomputeFlow
               . resetAll

-- @return the given GameState, with all DisplayTiles re-rendered as a
--         function of their respective Squares.
recomputeDisplays :: GameState -> GameState
recomputeDisplays gs = gs & board . each %~ makeDisplayTile
  where
    makeDisplayTile :: Square -> Square
    makeDisplayTile s = s & displaytile .~ d_t
      where d_t = DisplayTile { _n = if s ^. tile ^. north then A else Z
                              , _e = if s ^. tile ^.  east then A else Z
                              , _w = if s ^. tile ^.  west then A else Z
                              , _s = if s ^. tile ^. south then A else Z
                              }



-- @return the given GameState, with all _visited and _flowstate fields updated
--         to reflect the new connectivity graph.
recomputeFlow :: GameState -> GameState
recomputeFlow gs = if entry_is_connected
                     then trickleFrom 0 entry_xy gs
                     else gs
  where
    entry_is_connected = gs ^. board ^?! ix entry_xy . tile . north
    entry_xy = (\(h,w) -> (h+1, w)) $ gs ^. border . tapLocation

    -- @return the given GameState, with all Squares adjacent to the the Square
    --         at the input coordinates explored / colored in.
    trickleFrom :: Int -> (Int, Int) -> GameState -> GameState
    trickleFrom n (h,w) gs = if n >= 100 || gs ^. board ^?! ix (h,w) . visited
                               then gs
                               else exploreNeighbors n (h,w)
                                    $ board . ix (h,w) . visited .~ True
                                    $ board . ix (h,w) . flowing .~ True
                                    $ gs
      where
        -- @return the given Gamestate, with all adjacent squares explored.
        exploreNeighbors :: Int -> (Int, Int) -> GameState -> GameState
        exploreNeighbors n loc gs = exploreDir N n loc
                                  $ exploreDir E n loc
                                  $ exploreDir S n loc
                                  $ exploreDir W n loc
                                  gs
          where
            -- @return the given GameState, with the adjacent square in the
            --         given direction tested for connectivity / colored in.
            exploreDir :: Dir -> Int -> (Int, Int) -> GameState -> GameState
            exploreDir dir n loc gs = if canReach (gs ^. board) loc dir
                                        then trickleFrom (succ n) (adj dir loc) gs
                                        else gs


-- @return the given GameState, with the location of the cursor set.
recomputeCursor :: GameState -> GameState
recomputeCursor gs = gs & board . ix (gs ^. cursor) . hascursor .~ True

-- @return the given GameState, with all rendering artifacts cleared.
resetAll :: GameState -> GameState
resetAll gs = gs & board . each . hascursor   .~ False
                 & board . each . flowing     .~ False
                 & board . each . visited     .~ False
                 & board . each . displaytile .~ DisplayTile Z Z Z Z
