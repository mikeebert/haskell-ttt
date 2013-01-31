module ConsoleRunnerTest where

import Test.Hspec
import Test.QuickCheck
import ConsoleRunner

blankBoard = ["1","2","3",
              "4","5","6",
              "7","8","9"]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ConsoleRunner" $ do
   it "formats board for display" $ do
    formatted blankBoard `shouldBe` "1 2 3\n4 5 6\n7 8 9\n" 

   it "game is not over if in progress" $ do
     gameOver blankBoard `shouldBe` False

   it "game is over if a player has won" $ do
     gameOver ["x","x","x",
               "4","5","6",
               "7","8","9"] `shouldBe` True

   it "game is over if a tie game" $ do
     gameOver ["x","o","x",
               "o","x","o",
               "x","o","x"] `shouldBe` True
