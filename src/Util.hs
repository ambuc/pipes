module Util where

import           Data.Maybe     (isJust)
import           Lens.Micro     ((%~), (&), (.~), (^.), (^?!))
import           Lens.Micro.GHC (ix)

import           Magic          (getBoardHeight, getBoardWidth)
import           Types

data Shape = Null | Nub | Line | Bend | Tee | Cross deriving (Bounded, Enum)

-- @return whether or not the input coordinate is within the game board
inBounds :: (Int, Int) -> Bool
inBounds (h,w) = h >= minY && h <= maxY && w >= minX && w <= maxX
  where
    minX = -1; maxX = getBoardWidth  -- [-1, maxX]
    minY = -1; maxY = getBoardHeight -- [-x, maxY]

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

has' :: Dir -> Tile -> Bool
has' N (x,_,_,_) = x
has' E (_,x,_,_) = x
has' W (_,_,x,_) = x
has' S (_,_,_,x) = x

hasNorth :: Tile -> Bool
hasNorth = has' N

hasSouth :: Tile -> Bool
hasSouth = has' S

-- @return the coordinates adjacent to the input coordinate in the given
--         direction.
adj :: Dir -> (Int, Int) -> (Int, Int)
adj N (h,w) = (max (-1) (h-1), w)
adj S (h,w) = (min getBoardHeight (h+1), w)
adj E (h,w) = (h, min getBoardWidth (w+1))
adj W (h,w) = (h, max (-1) (w-1))

cursorAdj :: Dir -> (Int, Int) -> (Int, Int)
cursorAdj N (h,w) = (max 0 (h-1), w)
cursorAdj S (h,w) = (min (getBoardHeight-1) (h+1), w)
cursorAdj E (h,w) = (h, min (getBoardWidth-1) (w+1))
cursorAdj W (h,w) = (h, max 0 (w-1))

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
                       ,    _isborder = False
                       }
mkBorderSquare = mkEmptySquare {_isborder = True }
-- N E W S
mkHorizontalBorderSquare = mkBorderSquare { _tile = (False, True, True, False) }
mkVerticalBorderSquare   = mkBorderSquare { _tile = (True, False, False, True) }
mkTLBorderSquare         = mkBorderSquare { _tile = (False, True, False, True) }
mkTRBorderSquare         = mkBorderSquare { _tile = (False, False, True, True) }
mkBLBorderSquare         = mkBorderSquare { _tile = (True, True, False, False) }
mkBRBorderSquare         = mkBorderSquare { _tile = (True, False, True, False) }
mkTapBorderSquare        = mkBorderSquare { _tile = (True, False, False, True) }
mkDrainBorderSquare      = mkBorderSquare { _tile = (True, True, True, False) }


tapYX :: GameState -> (Int, Int)
tapYX gs = gs ^. tap

tapOutletYX :: GameState -> (Int, Int)
tapOutletYX gs = (t_y + 1, t_x)
  where t@(t_y, t_x) = gs ^. tap

drainInletYX :: GameState -> (Int, Int)
drainInletYX gs = (d_y - 1, d_x)
  where d@(d_y, d_x) = gs ^. drain

isComplete :: GameState -> Bool
isComplete gs = isJust (drain_inlet ^. distance)
             && hasSouth (drain_inlet ^. tile)
  where drain_inlet = gs ^. board ^?! ix (drainInletYX gs)
