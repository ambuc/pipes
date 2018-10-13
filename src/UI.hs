module UI
    ( draw
    ) where

import           Brick.Types                (Widget)
import           Brick.Widgets.Border       (borderWithLabel, vBorder)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center       (center)
import           Brick.Widgets.Core         (str, withBorderStyle, (<+>))

import           Types                      (GameState, Resource (..))

draw :: GameState -> [Widget Resource]
draw _ = [ withBorderStyle unicode
           $ borderWithLabel (str "Hello!")
           $ center (str "Left") <+> vBorder <+> center (str "Right")
           ]

