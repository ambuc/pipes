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
-- FLOW
--

-- N E W S
addFlowFrom :: Dir -> Maybe Flow -> Maybe Flow
addFlowFrom N Nothing               = Just $ Flow In Out Out Out
addFlowFrom E Nothing               = Just $ Flow Out In Out Out
addFlowFrom W Nothing               = Just $ Flow Out Out In Out
addFlowFrom S Nothing               = Just $ Flow Out Out Out In
addFlowFrom N (Just (Flow _ b c d)) = Just $ Flow In b c d
addFlowFrom E (Just (Flow a _ c d)) = Just $ Flow a In c d
addFlowFrom W (Just (Flow a b _ d)) = Just $ Flow a b In d
addFlowFrom S (Just (Flow a b c _)) = Just $ Flow a b c In

--
-- TILES / DISPLAYTILES
--

-- @return a rotated Tile
rotate :: Wise -> Tile -> Tile
rotate CW (Tile a b c d)  = Tile d a b c
rotate CCW (Tile a b c d) = Tile b c d a

-- @return the canonical empty Tile.
nullTile = Tile False False False False

mkPlainTile :: Tile -> DisplayTile
mkPlainTile (Tile a b c d) = DisplayTile (if a then A else Z)
                                         (if b then A else Z)
                                         (if c then A else Z)
                                         (if d then A else Z)
mkBoldTile :: Tile -> DisplayTile
mkBoldTile (Tile a b c d) = DisplayTile (if a then B else Z)
                                        (if b then B else Z)
                                        (if c then B else Z)
                                        (if d then B else Z)

mkWaterTile :: Int -> Int-> Tile -> DisplayTile
mkWaterTile time d t
  | time `mod` 20 == d `mod` 20  = mkBoldTile t
  | otherwise                    = mkPlainTile t

mkDisplayTile :: Int -> Square -> DisplayTile
mkDisplayTile t (sq @ Square { _distance = Nothing }) = mkPlainTile (sq ^. tile)
mkDisplayTile t (sq @ Square { _distance = Just d  }) = mkWaterTile t d (sq ^. tile)

-- @return the Square with its DisplayTile updated to reflect the other associated data.
setDisplayTile :: Int -> Square -> Square
setDisplayTile c sq = sq & displaytile .~ mkDisplayTile c sq

--------- --------- --------- --------- --------- --------- --------- ---------
-- ADJACENCY LOGIC  --------- --------- --------- --------- --------- ---------
--------- --------- --------- --------- --------- --------- --------- ---------

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
canReach' t t' N = t ^. tN && t' ^. tS
canReach' t t' S = t ^. tS && t' ^. tN
canReach' t t' W = t ^. tW && t' ^. tE
canReach' t t' E = t ^. tE && t' ^. tW
