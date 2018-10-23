module Magic where

-- If you're gonna have magic constants,
-- at least make them Magic constants.
getBChanQueueLength =     10 :: Int
getBoardBounds      = (getBoardHeight, getBoardWidth) :: (Int, Int)
getBoardHeight      =     10 :: Int
getBoardWidth       =     20 :: Int
getFrameRate        =  50000 :: Int
getMaxExploreDist   =    300 :: Int
getSyncRate         =     50 :: Int
