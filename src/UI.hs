module UI
    ( draw
    ) where

import           Brick.AttrMap              (attrName)
import           Brick.Types                (Widget)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center       (center)
import           Brick.Widgets.Core         (fill, hBox, hLimit, str, vBox,
                                             vLimit, withAttr, withBorderStyle,
                                             (<+>), (<=>))
import           Data.Array                 ((!))

import           Lens.Micro                 ((^.))

import           Magic
import           Types
import           Util

-- @return the rendered GameState.
draw :: GameState -> [Widget ()]
draw gs = [ withBorderStyle unicode
          $ center $ drawGameState gs <=> debugLine
          ]
  where
    debugLine = str $ show $ gs ^. time

-- @return the rendered Board, containing the Border and pipes at the center.
drawGameState :: GameState -> Widget ()
drawGameState gs = hBox [ corner_tl
                        , border_line tap_x
                        , tap
                        , border_line (getBoardWidth - tap_x - 1)
                        , corner_tr
                        ]
               <=> hBox [ border_col getBoardHeight
                        , drawBoard t (gs ^. board)
                        , border_col getBoardHeight
                        ]
               <=> hBox [ corner_bl
                        , border_line drain_x
                        , drain
                        , border_line (getBoardWidth - drain_x - 1)
                        , corner_br
                        ]
  where
    t = gs ^. time
    (_,   tap_x) = gs ^. (border . tapLocation)
    (_, drain_x) = gs ^. (border . drainLocation)
    tap   = withAttr (attrName "fg-blue") $ str "┳"
    drain = str "┻"
    corner_tl = str "┏"
    corner_tr = str "┓"
    corner_bl = str "┗"
    corner_br = str "┛"
    border_line n = hLimit n $ vLimit 1 $ fill '━'
    border_col  n = vLimit n $ hLimit 1 $ fill '┃'


-- @return the rendered Board.
drawBoard :: Int -> Board -> Widget ()
drawBoard t b = hLimit getBoardWidth
              $ vLimit getBoardHeight
              $ vBox $ map (drawRow b) [0..getBoardHeight-1]
  where
    -- @return the row at the given index.
    drawRow :: Board -> Int -> Widget ()
    drawRow b h = hBox $ map (drawSquareAt b h) [0..getBoardWidth-1]

    -- @return the square at the given coordinates.
    drawSquareAt :: Board -> Int -> Int -> Widget ()
    drawSquareAt b h w = drawSquare $ b ! (h,w)

    -- @return the rendered Square.
    drawSquare :: Square -> Widget ()
    drawSquare (s @ Square { _hascursor = True
                           ,      _tile = (False, False, False, False) }) = withAttr (attrName "fg-red")  $ str "░"
    drawSquare (s @ Square { _hascursor = True                         }) = withAttr (attrName "fg-red")  $ str $ showTile t s
    drawSquare (s @ Square {  _distance = Nothing                      }) =                                 str $ showTile t s
    drawSquare (s @ Square {  _distance = Just n                       }) = withAttr (attrName "fg-blue") $ str $ showTile t s


-- TILES

-- @return the described Tile, with f as the default fill.
mkTile :: Fill -> Tile -> DisplayTile
mkTile f (n, e, w, s) = ( if n then f else Z , if e then f else Z
                        , if w then f else Z , if s then f else Z
                        )

-- @return the described flooded $tile, at time $time, at distance $dist,
--         with flow characteristics $flow.
mkWaterTile :: Int -> Int -> Flow -> Tile -> DisplayTile
mkWaterTile time dist flow tile
  | (time `mod` sr) == (dist + 0 `mod` sr) =  mkFlowTile In flow tile
  | (time `mod` sr) == (dist + 1 `mod` sr) =  mkTile B tile
  | (time `mod` sr) == (dist + 2 `mod` sr) =  mkTile B tile
  | (time `mod` sr) == (dist + 3 `mod` sr) =  mkFlowTile Out flow tile
  | otherwise                           =  mkTile A tile
  where
    sr = getSyncRate
    mkFlowTile :: FlowDir -> Flow -> Tile -> DisplayTile
    mkFlowTile dir (fn, fe, fw, fs) (n, e, w, s) = ( mkFill dir fn n, mkFill dir fe e
                                                   , mkFill dir fw w, mkFill dir fs s
                                                   )
      where
        mkFill :: FlowDir -> FlowDir -> Bool -> Fill
        mkFill _ _ False = Z
        mkFill a b _     = if a == b then A else B

-- @return the described Square at time $time.
mkDisplayTile :: Int -> Square -> DisplayTile
mkDisplayTile time (square @ Square { _distance = Nothing   }) = mkTile A (square ^. tile)
mkDisplayTile time (square @ Square { _distance = Just dist
                                    ,     _flow = Just flow }) = mkWaterTile time dist flow (square ^. tile)

showTile :: Int -> Square -> String
showTile time square = [corpus !! (27 * fromEnum w + 9 * fromEnum s
                                  + 3 * fromEnum e + 1 * fromEnum n)]
  where (n, e, w, s) = mkDisplayTile time square
