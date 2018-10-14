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

import           Types
import           Util

-- @return the rendered GameState.
draw :: GameState -> [Widget Resource]
draw gs = [ withBorderStyle unicode
          $ center $ drawGameState gs <=> debugLine
          ]
  where
    debugLine = str $ show (gs ^. cursor)

-- @return the rendered Board, containing the Border and pipes at the center.
drawGameState :: GameState -> Widget Resource
drawGameState gs = hBox [ corner_tl
                        , border_line tap_x
                        , tap
                        , border_line (getBoardWidth - tap_x - 1)
                        , corner_tr
                        ]
               <=> hBox [ border_col getBoardHeight
                        , drawBoard (gs ^. board)
                        , border_col getBoardHeight
                        ]
               <=> hBox [ corner_bl
                        , border_line drain_x
                        , drain
                        , border_line (getBoardWidth - drain_x - 1)
                        , corner_br
                        ]
  where
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
drawBoard :: Board -> Widget Resource
drawBoard b = hLimit getBoardWidth
            $ vLimit getBoardHeight
            $ vBox $ map (drawRow b) [0..getBoardHeight-1]
  where
    -- @return the row at the given index.
    drawRow :: Board -> Int -> Widget Resource
    drawRow b h = hBox $ map (drawSquareAt b h) [0..getBoardWidth-1]

    -- @return the square at the given coordinates.
    drawSquareAt :: Board -> Int -> Int -> Widget Resource
    drawSquareAt b h w = drawSquare $ b ! (h,w)

    -- @return the rendered Square.
    drawSquare :: Square -> Widget Resource
    drawSquare s@Square { _hascursor = True }
      | s ^. tile == nullTile = withAttr (attrName "fg-red") $ str "░"
      | otherwise             = withAttr (attrName "fg-red") $ str $ show (s ^. displaytile)
    drawSquare s
      | s ^. connected = withAttr (attrName "fg-blue")
                       $ str $ show (s ^. displaytile)
      | otherwise      = str $ show (s ^. displaytile)


