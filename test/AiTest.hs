module AiTest where

import Test.Hspec
import Test.QuickCheck
import Player
import Board
import Ai

main :: IO ()
main = hspec spec

wonXBoard = ["x","x","x","4","5","6","7","8","9"]
wonOBoard = ["1","2","3","4","5","6","o","o","o"]

drawGameBoard = ["x","o","x",
                 "x","o","x",
                 "o","x","o"]

maxXplayer = Player {piece = "x", strategy = "max"}
minOplayer = Player {piece = "o", strategy = "min"}

spec :: Spec
spec = do
  describe "Ai" $ do
    it "returns first available move" $ do
      firstAvailableMove ["1","2","3"] `shouldBe` "1"

    it "returns the value of a won board in 1 move" $ do
      value wonXBoard maxXplayer minOplayer 1 `shouldBe` 99

    it "returns the value of a lost board in 1 move" $ do
      value wonOBoard maxXplayer minOplayer 1 `shouldBe` -99

    it "returns 0 as the value for a draw game" $ do
      value drawGameBoard maxXplayer minOplayer 9 `shouldBe` 0

    it "returns -1 as the value for a game in progress" $ do
      value ["1","2","3","4","5","7","8","9"] maxXplayer minOplayer 0 `shouldBe` -1

    it "returns move with highest score" $ do
      bestMove [(95,"1"), (-99,"2"), (-1,"3"), (96,"4"), (-1,"5")] `shouldBe` "4" 

    it "returns best score for max" $ do
      bestScore maxXplayer [0,1,99,-5] `shouldBe` 99

    it "returns the best score min" $ do
      bestScore minOplayer [1,0,-1,-5] `shouldBe` -5

    it "scores a won board for the max player" $ do
      minimaxScore maxXplayer minOplayer 5 wonXBoard `shouldBe` 95

    it "scores a won board for the min player" $ do
      minimaxScore maxXplayer minOplayer 2 wonOBoard `shouldBe` -98

    it "scores a tie game as 0" $ do
      minimaxScore maxXplayer minOplayer 8 drawGameBoard `shouldBe` 0

    it "scores a negative score if winning move left open for opponent" $ do
      minimaxScore maxXplayer minOplayer 1 ["o","o","3","x","x","o","x","o","x"] `shouldBe` -98

    it "chooses a win in one move" $ do
      getAiMove ["x","x","3","4","5","6","7","8","9"] maxXplayer minOplayer 
        `shouldBe` "3"

    {-it "ranks potential wins, losses and draws" $ do-}
      {-map (minimaxScore maxXplayer minOplayer 1)  -}
          {-(map (makeMove ["x","x","3","o","5","o","o","o","9"] (piece maxXplayer))-}
               {-(availableSpaces ["x","x","3","o","5","x","o","o","9"]))-}
      {-`shouldBe` [99,-98,-98]-}

    it "chooses the center for second move if opponent chooses corner" $ do
      getAiMove ["o","2","3","4","5","6","7","8","9"] maxXplayer minOplayer 
        `shouldBe` "5"

    it "blocks L setup" $ do
      moveIsInList (getAiMove ["o","2","3",
                               "4","x","6",
                               "7","o","9"] maxXplayer minOplayer)
        ["4","7","6","9"] `shouldBe` True

moveIsInList n list = length (filter (== n) list) >= 1

