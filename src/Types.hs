module Types where

data Resource = SomeResource deriving (Eq, Show, Ord)
data GameState = GameState {} deriving (Show)

corpus :: String

-- N      012012012012012012012012012012012012012012012012012012012012012012012012012012012
-- E      0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--0--1--2--
-- S      0--------1--------2--------0--------1--------2--------0--------1--------2--------
-- W      0--------------------------1--------------------------2--------------------------
corpus = " ╵╹╶└┖╺┕┗╷│╿┌├┞┍┝┡╻╽┃┎┟┠┏┢┣╴┘┚─┴┸╼┶┺┐┤┦┬┼╀┮┾╄┒┧┨┰╁╂┲╆╊╸┙┛╾┵┹━┷┻┑┥┩┭┽╃┯┿╇┓┪┫┱╅╉┳╈╋"

data Fill = Z | A | B deriving (Show, Bounded, Ord, Eq, Enum) -- zilch, average, bold
data Tile = Tile { north :: Fill, east :: Fill, south :: Fill, west :: Fill }

instance Show Tile where
  show (Tile n e w s) = [corpus !! (27 * fromEnum s
                                   + 9 * fromEnum w
                                   + 3 * fromEnum e
                                   + 1 * fromEnum n)]

data Shape = Line | Bend | Tee | X | Culdesac

-- a ─ │      <=> ━ ┃      (and intermediates)
-- b  ┌ ┐ └ ┘ <=>  ┏ ┓ ┗ ┛
-- c  ├ ┬ ┤ ┴ <=>  ┣ ┳ ┫ ┻
-- d ┼        <=> ╋
-- e ╴ ╵ ╶ ╷  <=> ╸ ╹ ╺ ╻
