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
import           Data.Array                 (assocs, elems)
import           Text.Wrap

import           Init                       (getBoardHeight, getBoardWidth)
import           Types

draw :: GameState -> [Widget Resource]
draw gs = [ withBorderStyle unicode
          $ center
          $ drawBoard gs
          ]

breakWords = defaultWrapSettings { breakLongWords = True }

-- /--+----\  <-- topBorder
-- |.......|
-- \-----+-/  <-- bottomBorder
drawBoard :: GameState -> Widget Resource
drawBoard gs = drawTopBorder
            <=> hBox [drawLeftBorder, drawPipes gs, drawRightBorder]
            <=> drawBottomBorder
  where
    drawTopBorder    = str $ "┏" ++ replicate tap_location '━' ++ "┳"
                                 ++ replicate (w - tap_location - 1) '━' ++ "┓"
    drawBottomBorder = str $ "┗" ++ replicate drain_location '━' ++ "┻"
                                 ++ replicate (w - drain_location - 1) '━' ++ "┛"
    drawLeftBorder   = hLimit 1 $ strWrapWith breakWords $ replicate h '┃'
    drawRightBorder  = hLimit 1 $ strWrapWith breakWords $ replicate h '┃'
    tap_location   = tapLocation   $ border gs
    drain_location = drainLocation $ border gs
    w = getBoardWidth
    h = getBoardHeight


drawPipes :: GameState -> Widget Resource
drawPipes gs = hLimit getBoardWidth $ vLimit getBoardHeight
             $ strWrapWith breakWords
             $ concatMap (\((i,j), e) -> if (i,j) == cursor gs
                                     then show e { isselected = True }
                                     else show e
                         )
             $ assocs $ board gs

