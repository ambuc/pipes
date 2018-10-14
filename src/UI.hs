module UI
    ( draw
    ) where

import           Brick.AttrMap              (AttrMap, attrMap, attrName)
import           Brick.Types                (Widget)
import           Brick.Widgets.Border       (borderWithLabel, vBorder)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center       (center)
import           Brick.Widgets.Core         (hBox, hLimit, str, strWrapWith,
                                             vBox, vLimit, withAttr,
                                             withBorderStyle, (<+>), (<=>))
import           Data.Array                 (assocs, elems, (!))
import           Text.Wrap

import           Lens.Micro                 (ix, (&), (^.))

import           Init                       (getBoardHeight, getBoardWidth)
import           Types

draw :: GameState -> [Widget Resource]
draw gs = [ withBorderStyle unicode
          $ center $ drawBoard gs <=> debugLine
          ]
  where
    debugLine = str $ show (gs ^. cursor)

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
    tap_location    = gs ^. (border . tapLocation)
    drain_location  = gs ^. (border . drainLocation)
    w = getBoardWidth
    h = getBoardHeight


drawPipes :: GameState -> Widget Resource
drawPipes gs = hLimit getBoardWidth
             $ vLimit getBoardHeight
             $ vBox $ map (drawRow gs) [0..getBoardHeight-1]

drawRow :: GameState -> Int -> Widget Resource
drawRow gs h = hBox $ map (drawSquare gs h) [0..getBoardWidth-1]

drawSquare :: GameState -> Int -> Int -> Widget Resource
drawSquare gs h w = drawTile (gs ^. cursor == (h,w)) t
  where t = ((gs ^. board) ! (h,w)) ^. tile

-- the bool represents whether or not this tile currently has the cursor over it.
drawTile :: Bool -> Tile -> Widget Resource
drawTile True (Tile Z Z Z Z) = withAttr (attrName "fg-red") $ str "░"
drawTile True t              = withAttr (attrName "fg-red") $ str $ show t
drawTile False t             = str $ show t


