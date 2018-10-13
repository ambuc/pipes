module Init
    ( mkState
    ) where

import qualified System.Random as R (StdGen, newStdGen, next)

import           Types         (GameState (..))

someFunc :: IO ()
someFunc = putStrLn "someFunc"

mkState :: R.StdGen -> GameState
mkState _ = GameState {}

