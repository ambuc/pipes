import           Control.Exception (evaluate)
import           Data.List         (sort)
import           Data.List.Unique  (repeated)
import           Test.Hspec
import           Test.QuickCheck

import           Types

main :: IO ()
main = hspec $ do
  describe "src/Types >> DisplayTile >> corpus" $ do
    it "no-corpus-duplicates" $ repeated corpus `shouldBe` []
    it "show-u-2500"          $ showTile (DisplayTile Z A Z A) `shouldBe` "─"
    it "show-u-2511"          $ showTile (DisplayTile Z Z A B) `shouldBe` "┑"
    it "show-u-2522"          $ showTile (DisplayTile A B B Z) `shouldBe` "┢"
    it "show-u-2533"          $ showTile (DisplayTile Z B B B) `shouldBe` "┳"
    it "show-u-2544"          $ showTile (DisplayTile B B A A) `shouldBe` "╄"
    it "show-u-2544"          $ showTile (DisplayTile B B A A) `shouldBe` "╄"
    it "show-u-2575"          $ showTile (DisplayTile A Z Z Z) `shouldBe` "╵"
    it "show-u-2516"          $ showTile (DisplayTile B A Z Z) `shouldBe` "┖"
    it "show-u-2527"          $ showTile (DisplayTile A Z B A) `shouldBe` "┧"
    it "show-u-2538"          $ showTile (DisplayTile B A Z A) `shouldBe` "┸"
    it "show-u-2549"          $ showTile (DisplayTile B A B B) `shouldBe` "╉"
    it "show-u-257A"          $ showTile (DisplayTile Z B Z Z) `shouldBe` "╺"
