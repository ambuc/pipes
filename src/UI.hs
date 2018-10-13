module UI
    ( draw
    ) where
import           Brick.Types                (Widget)
import           Brick.Widgets.Border       (borderWithLabel, vBorder)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center       (center)
import           Brick.Widgets.Core         (hLimit, str, strWrapWith, vLimit,
                                             withBorderStyle, (<+>))
import           Data.Array                 (elems)
import           Text.Wrap

import           Init                       (getBoardHeight, getBoardWidth)
import           Types                      (GameState (..), Resource (..))

draw :: GameState -> [Widget Resource]
draw game_state = [ withBorderStyle unicode
                  $ center $ drawBoard game_state ]

drawBoard :: GameState -> Widget Resource
drawBoard game_state = hLimit getBoardWidth $ vLimit getBoardHeight
                     $ strWrapWith wrap_settings
                       $ concatMap show $ elems $ board game_state
  where wrap_settings = defaultWrapSettings { breakLongWords = True }

