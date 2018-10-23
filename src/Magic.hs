module Magic where

-- If you're gonna have magic constants,
-- at least make them Magic constants.

-- Brick can only cache so many inputs at once. This limits how many keystrokes
-- are held and executed upon.
getBChanQueueLength =     10 :: Int

-- Helper function for getBoardHeight and getBoardWidth.
getBoardBounds      = (getBoardHeight, getBoardWidth) :: (Int, Int)

-- Hard-coded board height and width. If changed, the game should still render.
getBoardHeight      =     10 :: Int
getBoardWidth       =     20 :: Int

-- Hard-coded framerate. Used in main's Brick bchan magic.
getFrameRate        =  50000 :: Int

-- Hard-coded max depth to explore, to prevent loops / weird edge cases. Should
-- be raised if board height/width are.
getMaxExploreDist   =    300 :: Int
