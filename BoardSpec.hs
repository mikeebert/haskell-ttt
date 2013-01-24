import Test.Hspec
import Test.QuickCheck
import Board

blankBoard = ['1','2','3','4','5','6','7','8','9']
fullBoard = ['x','o','x','x','o','x','o','x','o']

main :: IO ()
main = hspec $ do
  describe "Board" $ do
    it "returns an empty board" $ do
      emptyBoard `shouldBe` ['1','2','3','4','5','6','7','8','9']

    it "places a move on an empty board" $ do
      placeMove ('1','x') emptyBoard 
        `shouldBe` ['x','2','3','4','5','6','7','8','9']

    it "places a move on a board with other moves" $ do
      placeMove ('4','x') ['o','x','o','4','x','o','x','x','9'] 
        `shouldBe` ['o','x','o','x','x','o','x','x','9']

    it "doesn't place a move in an occupied space" $ do
      placeMove ('2','x') ['1','o','3','4','5','6','7','8','9']
        `shouldBe` ['1','o','3','4','5','6','7','8','9']

    it "returns a list of available spaces on empty board" $ do
      availableSpaces emptyBoard 
        `shouldBe` ['1','2','3','4','5','6','7','8','9']

    it "returns True for a full board" $ do
      full fullBoard `shouldBe` True

    it "returns winner for horizontal win in first row" $ do
      winner ['x','x','x','4','5','6','7','8','9'] `shouldBe` 'x'

    it "returns winner for horizontal win in second row" $ do
      winner ['1','2','3','x','x','x','7','8','9'] `shouldBe` 'x'

    it "returns winner for horizontal win in final row" $ do
      winner ['1','2','3','4','5','6','x','x','x'] `shouldBe` 'x'

    it "returns nil for no winner" $ do
      winner blankBoard `shouldBe` '0'
    
