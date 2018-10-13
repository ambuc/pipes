import           Control.Exception (evaluate)
import           Data.List         (sort)
import           Data.List.Unique  (repeated)
import           Test.Hspec
import           Test.QuickCheck

import           Types

main :: IO ()
main = hspec $ do
  describe "src/Types >> corpus" $ do
    it "no-corpus-duplicates" $ repeated corpus `shouldBe` []
    it "show-u-2500"          $ show (Tile Z A Z A) `shouldBe` "─"
    it "show-u-2511"          $ show (Tile Z Z A B) `shouldBe` "┑"
    it "show-u-2522"          $ show (Tile A B B Z) `shouldBe` "┢"
    it "show-u-2533"          $ show (Tile Z B B B) `shouldBe` "┳"
    it "show-u-2544"          $ show (Tile B B A A) `shouldBe` "╄"
    it "show-u-2544"          $ show (Tile B B A A) `shouldBe` "╄"
    it "show-u-2575"          $ show (Tile A Z Z Z) `shouldBe` "╵"
    it "show-u-2516"          $ show (Tile B A Z Z) `shouldBe` "┖"
    it "show-u-2527"          $ show (Tile A Z B A) `shouldBe` "┧"
    it "show-u-2538"          $ show (Tile B A Z A) `shouldBe` "┸"
    it "show-u-2549"          $ show (Tile B A B B) `shouldBe` "╉"
    it "show-u-257A"          $ show (Tile Z B Z Z) `shouldBe` "╺"
  describe "src/Types >> Tiles" $ do
    it "tile-rotation-cw" $ do
      let t = Tile Z B B A
      show t `shouldBe` "┲"
      show (rotateCW t) `shouldBe` "┪"
    it "tile-rotation-ccw" $ do
      let t = Tile A B A Z
      show t `shouldBe` "┝"
      show (rotateCCW t) `shouldBe` "┸"
    it "tile-rotation-roundtrip" $ do
      let t = Tile A B Z A
      (rotateCW . rotateCW . rotateCW . rotateCW) t `shouldBe` t
      (rotateCCW . rotateCCW . rotateCCW . rotateCCW) t `shouldBe` t

