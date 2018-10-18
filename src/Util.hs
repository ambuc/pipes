module Util where

import           Lens.Micro     ((%~), (&), (.~), (^.), (^?!))
import           Lens.Micro.GHC (ix)

import           Magic          (getBoardHeight, getBoardWidth)
import           Types

-- @return whether or not the input coordinate is within the game board
inBounds :: (Int, Int) -> Bool
inBounds (h,w) = h >= 0 && h < getBoardHeight && w >= 0 && w < getBoardWidth

-- N E W S
addFlowFrom :: Dir -> Maybe Flow -> Maybe Flow
addFlowFrom N Nothing             = Just (In,  Out, Out, Out)
addFlowFrom E Nothing             = Just (Out,  In, Out, Out)
addFlowFrom W Nothing             = Just (Out, Out,  In, Out)
addFlowFrom S Nothing             = Just (Out, Out, Out,  In)
addFlowFrom N (Just (_, b, c, d)) = Just ( In,   b,   c,   d)
addFlowFrom E (Just (a, _, c, d)) = Just (  a,  In,   c,   d)
addFlowFrom W (Just (a, b, _, d)) = Just (  a,   b,  In,   d)
addFlowFrom S (Just (a, b, c, _)) = Just (  a,   b,   c,  In)

isNullTile :: Tile -> Bool
isNullTile = (==(False, False, False, False))

inv :: Dir -> Dir
inv N = S
inv S = N
inv E = W
inv W = E

-- @return a rotated Tile
rotate (n, e, w, s) = (w, n, s, e)

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
canReach' (n,_,_,_) (_,_,_,s) N = n && s
canReach' (_,e,_,_) (_,_,w,_) E = e && w
canReach' (_,_,w,_) (_,e,_,_) W = w && e
canReach' (_,_,_,s) (n,_,_,_) S = s && n
