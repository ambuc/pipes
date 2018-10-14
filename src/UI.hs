module UI
    ( draw
    ) where
import           Brick.Types                (Widget)
import           Brick.Widgets.Border       (borderWithLabel, vBorder)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center       (center)
import           Brick.Widgets.Core         (hBox, hLimit, str, strWrapWith,
                                             vBox, vLimit, withBorderStyle,
                                             (<+>), (<=>))
import           Data.Array                 (elems)
import           Text.Wrap

import           Init                       (getBoardHeight, getBoardWidth)
import           Types

draw :: GameState -> [Widget Resource]
draw game_state = [ withBorderStyle unicode
                  $ center
                  $ drawBoard game_state
                  ]

breakWords = defaultWrapSettings { breakLongWords = True }

-- /--+----\  <-- topBorder
-- |.......|
-- \-----+-/  <-- bottomBorder
drawBoard :: GameState -> Widget Resource
drawBoard gs = drawTopBorder gs
            <=> hBox [drawLeftBorder gs, drawPipes gs, drawRightBorder gs]
            <=> drawBottomBorder gs
  where
    drawTopBorder    gs = str $ "┏" ++ replicate tap_location '━' ++ "┳" ++ replicate (w - tap_location - 1) '━' ++ "┓"
    drawBottomBorder gs = str $ "┗" ++ replicate tap_location '━' ++ "┻" ++ replicate (w - tap_location - 1) '━' ++ "┛"
    drawLeftBorder  gs = hLimit 1 $ strWrapWith breakWords $ replicate h '┃'
    drawRightBorder gs = hLimit 1 $ strWrapWith breakWords $ replicate h '┃'
    tap_location   = tapLocation $ border gs
    drain_location = drainLocation $ border gs
    w = getBoardWidth
    h = getBoardHeight


drawPipes :: GameState -> Widget Resource
drawPipes game_state = hLimit getBoardWidth $ vLimit getBoardHeight
                     $ strWrapWith breakWords
                       $ concatMap show $ elems $ board game_state

