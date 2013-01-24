import Test.Hspec
import Test.QuickCheck
import Board

blankBoard = "123456789"
fullBoard = ['x','o','x','x','o','x','o','x','o']

main :: IO ()
main = hspec $ do
  describe "Board" $ do
    it "returns an empty board" $ do
      emptyBoard `shouldBe` "123456789"

    it "places a move on an empty board" $ do
      placeMove ('1','x') emptyBoard `shouldBe` "x23456789"

    it "places a move on a board with other moves" $ do
      placeMove ('4','x') "oxo4xoxx9" `shouldBe` "oxoxxoxx9"

    it "doesn't place a move in an occupied space" $ do
      placeMove ('2','x') "1o3456789" `shouldBe` "1o3456789"

    it "returns a list of available spaces on empty board" $ do
      availableSpaces emptyBoard `shouldBe` "123456789"

    it "returns True for a full board" $ do
      full fullBoard `shouldBe` True

    it "returns winner for horizontal win in first row" $ do
      winner "xxx456789" `shouldBe` 'x'

    it "returns winner for horizontal win in second row" $ do
      winner "123xxx789" `shouldBe` 'x'

    it "returns winner for horizontal win in final row" $ do
      winner "123456xxx" `shouldBe` 'x'

    it "returns nil for no winner" $ do
      winner blankBoard `shouldBe` '0'
    
