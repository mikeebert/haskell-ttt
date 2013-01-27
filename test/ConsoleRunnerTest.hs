module ConsoleRunnerTest where

import Test.Hspec
import Test.QuickCheck
import ConsoleRunner

blankBoard = ["1","2","3","4","5","6","7","8","9"]
fullBoard =  ["x","o","x","x","o","x","o","x","o"]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ConsoleRunner" $ do
   it "formats board for display" $ do
    formatted blankBoard `shouldBe` "1 2 3\n4 5 6\n7 8 9\n" 
