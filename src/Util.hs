module Util where

import           Data.Maybe     (isJust)
import           Lens.Micro     ((%~), (&), (.~), (^.), (^?!))
import           Lens.Micro.GHC (ix)

import           Magic          (getBoardHeight, getBoardWidth)
import           Types

data Shape = Null | Nub | Line | Bend | Tee | Cross deriving (Bounded, Enum)

-- @return whether or not the input coordinate is within the game board
inBounds :: (Int, Int) -> Bool
inBounds (h,w) = h >= 0 && h < getBoardHeight && w >= 0 && w < getBoardWidth

-- N E W S
-- @returns a conditional flow with the given direction added to its inlet set.
addFlowFrom :: Dir -> Maybe Flow -> Maybe Flow
addFlowFrom N Nothing             = Just (In,  Out, Out, Out)
addFlowFrom E Nothing             = Just (Out,  In, Out, Out)
addFlowFrom W Nothing             = Just (Out, Out,  In, Out)
addFlowFrom S Nothing             = Just (Out, Out, Out,  In)
addFlowFrom N (Just (_, b, c, d)) = Just ( In,   b,   c,   d)
addFlowFrom E (Just (a, _, c, d)) = Just (  a,  In,   c,   d)
addFlowFrom W (Just (a, b, _, d)) = Just (  a,   b,  In,   d)
addFlowFrom S (Just (a, b, c, _)) = Just (  a,   b,   c,  In)

-- @return the canonical null tile.
isNullTile :: Tile -> Bool
isNullTile = (==(False, False, False, False))

-- @return a Tile of the given Shape.
shapeToTile :: Shape -> Tile -- N E W S
shapeToTile Null  = (False, False, False, False)  -- 0 ' '
shapeToTile Nub   = ( True, False, False, False)  -- 1 '╵'
shapeToTile Line  = (False,  True,  True, False)  -- 2 '─'
shapeToTile Bend  = ( True,  True, False, False)  -- 3 '└'
shapeToTile Tee   = ( True,  True, False,  True)  -- 4 '├'
shapeToTile Cross = ( True,  True,  True,  True)  -- 5 '┼'

-- @return the inversion of the given direction.
inv :: Dir -> Dir
inv N = S
inv S = N
inv E = W
inv W = E

-- @return a rotated Tile
rotate (n, e, w, s) = (w, n, s, e)

hasSouth :: Tile -> Bool
hasSouth (_,_,_,x) = x

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

-- @return an empty Square.
mkEmptySquare :: Square
mkEmptySquare = Square {        _tile = (False, False, False, False)
                       ,        _flow = Nothing
                       ,    _distance = Nothing
                       ,   _hascursor = False
                       }

isComplete :: GameState -> Bool
isComplete gs = isJust (drain_inlet ^. distance)
             && hasSouth (drain_inlet ^. tile)
  where
    drain_inlet = gs ^. board ^?! ix di
    di@(di_y, di_x) = (d_y-1, d_x) -- drain inlet
    d@(d_y, d_x) = gs ^. border ^. drainLocation
