module Util where

import           Lens.Micro     ((%~), (&), (.~), (^.), (^?!))
import           Lens.Micro.GHC (each, ix)

import           Types

--
-- BOARD
--

getBoardWidth = 20
getBoardHeight = 10

-- @return whether or not the input coordinate is within the game board
inBounds :: (Int, Int) -> Bool
inBounds (h,w) = h >= 0 && h < getBoardHeight && w >= 0 && w < getBoardWidth

--
-- TILES / DISPLAYTILES
--

-- @return the canonical empty Tile.
nullTile = Tile False False False False

-- @return the Square with its DisplayTile updated to reflect the other associated data.
makeDisplayTile :: Square -> Square
makeDisplayTile s = s & displaytile .~ d_t
  where d_t = DisplayTile { _n = if s ^. tile ^. north then A else Z
                          , _e = if s ^. tile ^.  east then A else Z
                          , _w = if s ^. tile ^.  west then A else Z
                          , _s = if s ^. tile ^. south then A else Z
                          }

--
-- ADJACENCY LOGIC
--

-- @return the coordinates adjacent to the input coordinate in the given
--         direction.
adj :: Dir -> (Int, Int) -> (Int, Int)
adj N (h,w) = (max 0 (h-1), w)
adj E (h,w) = (h, min (getBoardWidth - 1) (w+1))
adj W (h,w) = (h, max 0 (w-1))
adj S (h,w) = (min (getBoardHeight - 1) (h+1), w)

-- @return whether or not the square at that location can reach the tile
--         adjacent to it in the given direction.
canReach :: Board -> (Int, Int) -> Dir -> Bool
canReach board xy dir = inBounds adj_xy
                 && canReach' (board ^?! ix xy . tile)
                              (board ^?! ix adj_xy . tile)
                              dir
  where adj_xy = adj dir xy

-- @return whether or not the two argument tiles share an interface.
canReach' :: Tile -> Tile -> Dir -> Bool
canReach' t t' N = t ^. north && t' ^. south
canReach' t t' S = t ^. south && t' ^. north
canReach' t t' W = t ^.  west && t' ^.  east
canReach' t t' E = t ^.  east && t' ^.  west
