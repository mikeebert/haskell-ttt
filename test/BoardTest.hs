module BoardTest where

import Test.Hspec
import Test.QuickCheck
import Board

blankBoard = ["1","2","3","4","5","6","7","8","9"]
fullBoard = ["x","o","x","x","o","x","o","x","o"]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Board" $ do
    it "returns an empty board" $ do
      emptyBoard `shouldBe` ["1","2","3","4","5","6","7","8","9"]

    it "places a move on an empty board" $ do
      placeMove ("1","x") emptyBoard 
      `shouldBe` ["x","2","3","4","5","6","7","8","9"]

    it "places a move on a board with other moves" $ do
      placeMove ("4","x") ["o","x","o","4","o","x","x","8","9"] 
        `shouldBe` ["o","x","o","x","o","x","x","8","9"]

    it "doesn't place a move in an occupied space" $ do
      placeMove ("2","x") fullBoard `shouldBe` fullBoard

    it "returns available spaces" $ do
      availableSpaces ["1","o","x","o","5","x","o","x","o"] `shouldBe` ["1","5"]

    it "returns available spaces on empty board" $ do
      availableSpaces emptyBoard `shouldBe` ["1","2","3","4","5","6","7","8","9"]

    it "returns an empty list of available spaces on a full board" $ do
      availableSpaces fullBoard `shouldBe` []

    it "#full returns True for a full board" $ do
      full fullBoard `shouldBe` True

    it "#full returns False for an empty board" $ do
      full emptyBoard `shouldBe` False

    it "returns board rows" $ do
      rows emptyBoard `shouldBe` [["1","2","3"],["4","5","6"],["7","8","9"]]

    it "returns board columns" $ do
      columns emptyBoard `shouldBe` [["1","4","7"],["2","5","8"],["3","6","9"]]

    it "returns winner for horizontal wins" $ do
      winner ["x","x","x","4","5","6","7","8","9"] `shouldBe` "x"
      winner ["1","2","3","x","x","x","7","8","9"] `shouldBe` "x"
      winner ["1","2","3","4","5","6","x","x","x"] `shouldBe` "x"

    it "returns winner for column wins" $ do
      winner ["x","2","3","x","5","6","x","8","9"] `shouldBe` "x"
      winner ["1","x","3","4","x","6","7","x","9"] `shouldBe` "x"
      winner ["1","2","x","4","5","x","7","8","x"] `shouldBe` "x"

    it "returns winner for diagonal wins" $ do
      winner ["1","2","x","4","x","6","x","8","9"] `shouldBe` "x"
      winner ["x","2","3","4","x","6","7","8","x"] `shouldBe` "x"

    it "returns null list for no winner" $ do
      null (winner emptyBoard) `shouldBe` True  

    it "#draw returns True for a full board with null winner" $ do
      tieGame fullBoard `shouldBe` True

    it "#draw returns for a won board" $ do
      tieGame ["x","x","x","4","5","6","7","8","9"] `shouldBe` False
