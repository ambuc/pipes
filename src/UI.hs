module UI
    ( render
    , redraw
    ) where

import           Brick.AttrMap              (attrName)
import           Brick.Types                (Widget)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center       (center)
import           Brick.Widgets.Core         (fill, hBox, hLimit, str, vBox,
                                             vLimit, withAttr, withBorderStyle,
                                             (<+>), (<=>))
import           Data.Array                 ((!))
import           Data.Maybe                 (isJust)
import           Lens.Micro                 ((%~), (&), (.~), (^.), (^?!))
import           Lens.Micro.GHC             (each, ix)

import           Magic
import           Types
import           Util

-- @return the rendered GameState.
render :: GameState -> [Widget ()]
render gs = [ withBorderStyle unicode
            $ center $ drawGameState' gs
            ]

-- @return the given GameState, with all visual elements re-rendered as a
--         function of the current cursor / tile configuration.
redraw :: GameState -> GameState
redraw = id
       . recomputeOver'
       . recomputeCursor'
       . recomputeFlow
       . resetAll'

--------------------------------------------------------------------------------

type DisplayTile = (Fill, Fill, Fill, Fill)
data Fill        = Z | A | B deriving (Bounded, Enum) -- zilch, average, bold

-- @return the described Tile, with f as the default fill.
mkTile' :: Fill -> Tile -> DisplayTile
mkTile' f (n, e, w, s) = ( if n then f else Z , if e then f else Z
                         , if w then f else Z , if s then f else Z
                         )

-- @return the described flooded $tile, at time $time, at distance $dist,
--         with flow characteristics $flow.
mkWaterTile' :: Int -> Int -> Flow -> Tile -> DisplayTile
mkWaterTile' time dist flow tile
  | (time `mod` sr) == (dist + 0 `mod` sr) = mkFlowTile In flow tile
  | (time `mod` sr) == (dist + 1 `mod` sr) = mkTile' B tile
  | (time `mod` sr) == (dist + 2 `mod` sr) = mkTile' B tile
  | (time `mod` sr) == (dist + 3 `mod` sr) = mkTile' B tile
  | (time `mod` sr) == (dist + 4 `mod` sr) = mkFlowTile Out flow tile
  | otherwise                              = mkTile' A tile
  where
    sr = getSyncRate
    mkFlowTile :: FlowDir -> Flow -> Tile -> DisplayTile
    mkFlowTile dir (fn, fe, fw, fs) (n, e, w, s) = ( mkFill dir fn n
                                                   , mkFill dir fe e
                                                   , mkFill dir fw w
                                                   , mkFill dir fs s
                                                   )

    mkFill :: FlowDir -> FlowDir -> Bool -> Fill
    mkFill _ _ False = Z
    mkFill a b _     = if a == b then A else B

corpus :: String
-- N      012012012012012012012012012012012012012012012012012012012012012012012012012012012
-- E      0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--
-- S      0--------1--------2--------0--------1--------2--------0--------1--------2--------
-- W      0--------------------------1--------------------------2--------------------------
corpus = " ╵╹╶└┖╺┕┗╷│╿┌├┞┍┝┡╻╽┃┎┟┠┏┢┣╴┘┚─┴┸╼┶┺┐┤┦┬┼╀┮┾╄┒┧┨┰╁╂┲╆╊╸┙┛╾┵┹━┷┻┑┥┩┭┽╃┯┿╇┓┪┫┱╅╉┳╈╋"

showTile' :: Int -> Square -> String
showTile' time sq = [corpus !! (27 * fromEnum w + 9 * fromEnum s
                               + 3 * fromEnum e + 1 * fromEnum n)]
  where
    (n, e, w, s) = showTile'' sq

    showTile'' :: Square -> DisplayTile
    showTile'' (Square { _distance = Nothing }) = mkTile' A (sq ^. tile)
    showTile'' (Square {     _flow = Nothing }) = mkTile' A (sq ^. tile)
    showTile'' (Square { _distance = Just d
                       ,     _flow = Just f  }) = mkWaterTile' time d f (sq ^. tile)

-- @return the rendered Board.
drawBoard' :: Int -> Board -> Widget ()
drawBoard' t b = hLimit getBoardWidth
              $ vLimit getBoardHeight
              $ vBox $ map (drawRow' b) [0..getBoardHeight-1]
  where
    -- @return the row at the given index.
    drawRow' :: Board -> Int -> Widget ()
    drawRow' b h = hBox $ map (drawSquareAt' b h) [0..getBoardWidth-1]

    -- @return the square at the given coordinates.
    drawSquareAt' :: Board -> Int -> Int -> Widget ()
    drawSquareAt' b h w = drawSquare' $ b ! (h,w)

    -- @return the rendered Square.
    drawSquare' :: Square -> Widget ()
    drawSquare' s = squareStyle' s $ squareContent' s

    -- @return the custom styling for the Square.
    squareStyle' :: Square -> Widget () -> Widget ()
    squareStyle' Square { _hascursor = True   } = (withAttr $ attrName "bg-red")
                                                . (withAttr $ attrName "fg-red")
    squareStyle' Square {  _distance = Just n } = withAttr $ attrName "fg-blue"
    squareStyle' _                              = id

    -- @return the text content of the Square.
    squareContent' :: Square -> Widget ()
    squareContent' sq
      | sq ^. hascursor && isNullTile (sq ^. tile) = str "░"
      | otherwise                                  = str $ showTile' t sq

-- @return the rendered Board, containing the Border and pipes at the center.
drawGameState' :: GameState -> Widget ()
drawGameState' gs = hBox [ corner_tl
                        , border_line tap_x
                        , tap
                        , border_line (getBoardWidth - tap_x - 1)
                        , corner_tr
                        ]
               <=> hBox [ border_col getBoardHeight
                        , drawBoard' t (gs ^. board)
                        , border_col getBoardHeight
                        ]
               <=> hBox [ corner_bl
                        , border_line drain_x
                        , drain
                        , border_line (getBoardWidth - drain_x - 1)
                        , corner_br
                        ]
  where
    t       = gs ^. time
    is_over = gs ^. over
    (_,   tap_x) = gs ^. (border . tapLocation)
    (_, drain_x) = gs ^. (border . drainLocation)
    tap   = but_blue $ str "┳"
    drain = maybe_blue $ str "┻"
    corner_tl = maybe_blue $ str "┏"
    corner_tr = maybe_blue $ str "┓"
    corner_bl = maybe_blue $ str "┗"
    corner_br = maybe_blue $ str "┛"
    border_line n = maybe_blue $ hLimit n $ vLimit 1 $ fill '━'
    border_col  n = maybe_blue $ vLimit n $ hLimit 1 $ fill '┃'

    maybe_blue = if (is_over) then (but_blue) else id
    but_blue = withAttr $ attrName "fg-blue"

-- @return the given GameState, with all _flowstate fields updated
--         to reflect the new connectivity graph.
recomputeFlow :: GameState -> GameState
recomputeFlow gs = gs & board %~ (if entry_is_connected
                                    then trickleFrom 1 S tap_outlet_xy
                                    else id)
  where
    entry_is_connected = hasNorth $ gs ^. board ^?! ix tap_outlet_xy . tile
    tap_outlet_xy = tapOutletXY gs

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
      | canReach b loc dir = trickleFrom (succ dist) dir neighbor_xy b
      | otherwise          = b
      where
        neighbor = b ^?! ix neighbor_xy
        neighbor_xy = adj dir loc

-- @return the given GameState, with the location of the cursor set.
recomputeCursor' :: GameState -> GameState
recomputeCursor' gs = gs & board . ix (gs ^. cursor) . hascursor .~ True

-- @return the given GameState, with the _over field filled in via
--         Util.isComplete.
recomputeOver' :: GameState -> GameState
recomputeOver' gs = gs & over .~ (isComplete gs)


-- @return the given GameState, with all rendering artifacts cleared.
resetAll' :: GameState -> GameState
resetAll' gs = gs & board . each %~ resetSquare
  where
    resetSquare :: Square -> Square
    resetSquare Square {_tile = t} = mkEmptySquare { _tile = t}

