module UI
    ( render
    , redraw
    ) where

import           Brick.AttrMap              (attrName)
import           Brick.Types                (Widget)
import           Brick.Widgets.Border       (hBorder, vBorder)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center       (center, hCenter, vCenter)
import           Brick.Widgets.Core         (emptyWidget, fill, hBox, hLimit,
                                             padLeftRight, str, vBox, vLimit,
                                             withAttr, withBorderStyle, (<+>),
                                             (<=>))
import           Data.Array                 (elems, (!))
import           Data.Maybe                 (fromMaybe, isJust)
import           Debug.Trace                (trace)
import           Lens.Micro                 ((%~), (&), (.~), (^.), (^?!))
import           Lens.Micro.GHC             (each, ix)

import           Magic
import           Types
import           Util

-- @return the rendered GameState.
render :: GameState -> [Widget ()]
render gs = [ withBorderStyle unicode
            $ center
            $ vLimit (getBoardHeight + 2) $ hLimit (getBoardWidth + 15)
            $ hBox
                [ drawGameState' gs
                , padLeftRight 1 vBorder
                , hLimit 10
                  $   str ("Moves " ++ show (gs ^. moves))
                  <=> hBorder
                  <=> str (" Time " ++ show (gs ^. timer `div` 20))
                  <=> if gs ^. over then new_game_instructions else emptyWidget
                ]
            ]
  where new_game_instructions = vBox [ hBorder
                                     , str "You win!"
                                     , str "Press:"
                                     , str " 1 - Easy"
                                     , str " 2 - Mid"
                                     , str " 3 - Hard"
                                     , str "for a new"
                                     , str "game."
                                     ]

-- @return the given GameState, with all visual elements re-rendered as a
--         function of the current cursor / tile configuration.
redraw :: GameState -> GameState
redraw gs = if gs ^. over
              then gs
              else ( recomputeTap'
                   . recomputeOver'
                   . recomputeCursor'
                   . recomputeMaxDist'
                   . recomputeFlow'
                   . resetAll'
                   ) gs

--------------------------------------------------------------------------------

type DisplayTile = (Fill, Fill, Fill, Fill)
data Fill        = Z | A | B
                   deriving (Bounded, Enum) -- zilch, average, bold, dashed(2,3,4)

-- @return the described Tile, with f as the default fill.
mkTile' :: Fill -> Tile -> DisplayTile
mkTile' f (n, e, w, s) = ( if n then f else Z , if e then f else Z
                         , if w then f else Z , if s then f else Z
                         )

-- @return the described flooded $tile, at time $time, at distance $dist,
--         with flow characteristics $flow.
mkWaterTile' :: Int -> Int -> Int -> Flow -> Tile -> DisplayTile
mkWaterTile' max_dist time dist flow tile
  | (time `mod` sr) == (dist + 0 `mod` sr) = mkFlowTile In flow tile
  | (time `mod` sr) == (dist + 1 `mod` sr) = mkTile' B tile
  | (time `mod` sr) == (dist + 2 `mod` sr) = mkTile' B tile
  | (time `mod` sr) == (dist + 3 `mod` sr) = mkTile' B tile
  | (time `mod` sr) == (dist + 4 `mod` sr) = mkFlowTile Out flow tile
  | otherwise                              = mkTile' A tile
  where
    sr = max_dist + 5 -- num frames
    -- @return the DisplayTile described by the given Flow superimposed on the
    --         given Tile.
    mkFlowTile :: FlowDir -> Flow -> Tile -> DisplayTile
    mkFlowTile dir (fn, fe, fw, fs) (n, e, w, s) = ( mkFill dir fn n
                                                   , mkFill dir fe e
                                                   , mkFill dir fw w
                                                   , mkFill dir fs s
                                                   )

    -- @return the Fill described by the incoming FlowDir, the outgoing FlowDir,
    --         and whether or not that leg of the Tile is present.
    mkFill :: FlowDir -> FlowDir -> Bool -> Fill
    mkFill _ _ False = Z
    mkFill a b _     = if a == b then A else B

-- Box-drawing characters:
-- https://en.wikipedia.org/wiki/Box-drawing_character
corpus :: String
-- N      012012012012012012012012012012012012012012012012012012012012012012012012012012012
-- E      0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--
-- S      0--------1--------2--------0--------1--------2--------0--------1--------2--------
-- W      0--------------------------1--------------------------2--------------------------
corpus = " ╵╹╶└┖╺┕┗╷│╿┌├┞┍┝┡╻╽┃┎┟┠┏┢┣╴┘┚─┴┸╼┶┺┐┤┦┬┼╀┮┾╄┒┧┨┰╁╂┲╆╊╸┙┛╾┵┹━┷┻┑┥┩┭┽╃┯┿╇┓┪┫┱╅╉┳╈╋"

-- Helper function for squareContent.
-- @return the string content of a Tile, given its distance from the tap, the
--         current time, and the Square. Uses showTile'' to compute the fills
--         of the given Square.
showTile' :: Int -> Int -> Square -> String
showTile' max_dist time sq = [corpus !! (27 * fromEnum w + 9 * fromEnum s
                                        + 3 * fromEnum e + 1 * fromEnum n)]
  where
    (n, e, w, s) = showTile'' sq

    -- Helper function for showTile'.
    -- @return the DisplayTile with all Fill properties for the given Square.
    showTile'' :: Square -> DisplayTile
    showTile'' Square { _distance = Nothing } = mkTile' A (sq ^. tile)
    showTile'' Square {     _flow = Nothing } = mkTile' A (sq ^. tile)
    showTile'' Square { _distance = Just d
                      ,     _flow = Just f  } = mkWaterTile' max_dist time d f (sq ^. tile)

-- @return the rendered Board.
drawBoard' :: Int -> Int -> Board -> Widget ()
drawBoard' max_dist t b = hLimit (getBoardWidth + 2)
                        $ vLimit (getBoardHeight + 2)
                        $ vBox $ map (drawRow' b) [-1..getBoardHeight]
  where
    -- @return the row at the given index.
    drawRow' :: Board -> Int -> Widget ()
    drawRow' b h = hBox $ map (drawSquareAt' b h) [-1..getBoardWidth]

    -- @return the square at the given coordinates.
    drawSquareAt' :: Board -> Int -> Int -> Widget ()
    drawSquareAt' b h w = drawSquare' $ b ! (h,w)

    -- @return the rendered Square.
    drawSquare' :: Square -> Widget ()
    drawSquare' s = squareStyle' s $ squareContent' s

    -- @return the custom styling for the Square.
    squareStyle' :: Square -> Widget () -> Widget ()
    squareStyle' Square { _hascursor = True   } = withAttr (attrName "bg-red")
                                                . withAttr (attrName "fg-red")
    squareStyle' Square {  _distance = Just n } = withAttr $ attrName "fg-blue"
    squareStyle' _                              = id

    -- @return the text content of the Square.
    squareContent' :: Square -> Widget ()
    squareContent' sq = if sq ^. hascursor && isNullTile (sq ^. tile)
                          then str "░"
                          else str $ showTile' max_dist t sq

-- @return the rendered Board, containing the Border and pipes at the center.
drawGameState' :: GameState -> Widget ()
drawGameState' gs = drawBoard' (gs ^. maxdist) (gs ^. time) (gs ^. board)

-- @return the given GameState, with the _maxdist field updated
--         with the new maximum value of any Square's _distance field, with
--         a default of zero.
recomputeMaxDist' :: GameState -> GameState
recomputeMaxDist' gs = gs & maxdist .~ new_max_dist
  where new_max_dist = fromMaybe 0 $ maximum $ map (^. distance)
                     $ elems (gs ^. board)

-- @return the given GameState, with all _flowstate fields updated
--         to reflect the new connectivity graph.
recomputeFlow' :: GameState -> GameState
recomputeFlow' gs = gs & board %~ trickleFrom 0 S (tapYX gs)
  where
    -- @return the given Board, with all Squares adjacent to the the Square
    --         at the input coordinates explored / colored in.
    trickleFrom :: Int -> Dir -> (Int, Int) -> Board -> Board
    trickleFrom dist dir (h,w) b
      | shouldVisit = b
      | otherwise   = exploreNeighbors dist (h,w)
                      $ ix (h,w) . flow %~ addFlowFrom dir
                      $ ix (h,w) . distance .~ Just dist
                      $ b
      where
        -- @return whether or not the adjacent cell should be visited.
        --         Implements some kind of A*, probably.
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
      | canReach b loc dir = trickleFrom (succ dist) dir neighbor_yx b
      | otherwise          = b
      where
        neighbor = b ^?! ix neighbor_yx
        neighbor_yx = adj dir loc

-- @return the given GameState, with the location of the cursor set.
recomputeCursor' :: GameState -> GameState
recomputeCursor' gs = gs & board . ix (gs ^. cursor) . hascursor .~ True

-- @return the given GameState, with the _over field filled in via
--         Util.isComplete.
recomputeOver' :: GameState -> GameState
recomputeOver' gs = gs & over .~ isComplete gs

-- @return the given GameState, except if the game has been won, the tap tile is
--         redrawn as a T.
recomputeTap' :: GameState -> GameState
recomputeTap' gs = if gs ^. over
                     then  gs & board
                         . ix (gs ^. tap)
                         . tile .~ (False, True, True, True)
                     else gs

-- @return the given GameState, with all rendering artifacts cleared.
resetAll' :: GameState -> GameState
resetAll' gs = gs & board . each %~ resetSquare
  where
    resetSquare :: Square -> Square
    resetSquare Square {_tile = t} = mkEmptySquare { _tile = t}

