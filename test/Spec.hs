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
    it "show-u-2500"          $ show (DisplayTile Z A Z A) `shouldBe` "─"
    it "show-u-2511"          $ show (DisplayTile Z Z A B) `shouldBe` "┑"
    it "show-u-2522"          $ show (DisplayTile A B B Z) `shouldBe` "┢"
    it "show-u-2533"          $ show (DisplayTile Z B B B) `shouldBe` "┳"
    it "show-u-2544"          $ show (DisplayTile B B A A) `shouldBe` "╄"
    it "show-u-2544"          $ show (DisplayTile B B A A) `shouldBe` "╄"
    it "show-u-2575"          $ show (DisplayTile A Z Z Z) `shouldBe` "╵"
    it "show-u-2516"          $ show (DisplayTile B A Z Z) `shouldBe` "┖"
    it "show-u-2527"          $ show (DisplayTile A Z B A) `shouldBe` "┧"
    it "show-u-2538"          $ show (DisplayTile B A Z A) `shouldBe` "┸"
    it "show-u-2549"          $ show (DisplayTile B A B B) `shouldBe` "╉"
    it "show-u-257A"          $ show (DisplayTile Z B Z Z) `shouldBe` "╺"
