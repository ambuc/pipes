import           Control.Exception (evaluate)
import           Data.List         (sort)
import           Data.List.Unique  (repeated)
import           Test.Hspec
import           Test.QuickCheck

import           Types

main :: IO ()
main = hspec $ do
  describe "src/Types >> corpus" $ do
    it "No duplicates in corpus" $ repeated corpus `shouldBe` []
    it "Show '─' (U+2500)"       $ show (Tile Z A Z A) `shouldBe` "─"
    it "Show '┑' (U+2511)"       $ show (Tile Z Z A B) `shouldBe` "┑"
    it "Show '┢' (U+2522)"       $ show (Tile A B B Z) `shouldBe` "┢"
    it "Show '┳' (U+2533)"       $ show (Tile Z B B B) `shouldBe` "┳"
    it "Show '╄' (U+2544)"       $ show (Tile B B A A) `shouldBe` "╄"
    it "Show '╄' (U+2544)"       $ show (Tile B B A A) `shouldBe` "╄"
    it "Show '╵' (U+2575)"       $ show (Tile A Z Z Z) `shouldBe` "╵"
    it "Show '┖' (U+2516)"       $ show (Tile B A Z Z) `shouldBe` "┖"
    it "Show '┧' (U+2527)"       $ show (Tile A Z B A) `shouldBe` "┧"
    it "Show '┸' (U+2538)"       $ show (Tile B A Z A) `shouldBe` "┸"
    it "Show '╉' (U+2549)"       $ show (Tile B A B B) `shouldBe` "╉"
    it "Show '╺' (U+257A)"       $ show (Tile Z B Z Z) `shouldBe` "╺"

