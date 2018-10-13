module Main where

import           Brick

import           Lib

ui :: Widget ()
ui = str "Hello, world!"

main :: IO ()
main = simpleMain ui
