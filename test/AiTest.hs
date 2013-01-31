module AiTest where

import Test.Hspec
import Test.QuickCheck
import Player
import Board
import Ai
import MinimaxPlayer

main :: IO ()
main = hspec spec

wonXBoard = ["x","x","x","4","5","6","7","8","9"]
wonOBoard = ["1","2","3","4","5","6","o","o","o"]

drawGameBoard = ["x","o","x",
                 "x","o","x",
                 "o","x","o"]

maxXplayer = MinimaxPlayer {symbol = "x", strategy = "max"}
minOplayer = MinimaxPlayer {symbol = "o", strategy = "min"}

xPlayer = Player {piece = "x", kind = "computer"}
oPlayer = Player {piece = "o", kind = "human"}

spec :: Spec
spec = do
  describe "Ai" $ do
    it "returns the value of a won board in 1 move" $ do
      value wonXBoard maxXplayer minOplayer 1 `shouldBe` 99

    it "returns the value of a lost board in 1 move" $ do
      value wonOBoard maxXplayer minOplayer 1 `shouldBe` -99

    it "returns 0 as the value for a draw game" $ do
      value drawGameBoard maxXplayer minOplayer 1 `shouldBe` 0

    it "returns -1 as the value for a game in progress" $ do
      value emptyBoard maxXplayer minOplayer 0 `shouldBe` -1

    it "returns move with highest score from value-move pairs" $ do
      bestMoveFrom [(95,"1"), (-99,"2"), (-1,"3"), (96,"4"), (-1,"5")] `shouldBe` "4" 

    it "returns best score for max from a list of scores" $ do
      bestScore maxXplayer [0,1,99,-5] `shouldBe` 99

    it "returns the best score min from a list of scores" $ do
      bestScore minOplayer [1,0,-1,-5] `shouldBe` -5

    it "scores a won board for the max X player" $ do
      let depth = 5
      minimaxScore maxXplayer minOplayer depth wonXBoard `shouldBe` 95

    it "scores a won board for the min O player" $ do
      let depth = 5
      minimaxScore maxXplayer minOplayer depth wonOBoard `shouldBe` -95

    it "scores a tie game as 0" $ do
      let depth = 4
      minimaxScore maxXplayer minOplayer depth drawGameBoard `shouldBe` 0

    it "scores a loss if winning move left open for opponent" $ do
      let depth = 1
      minimaxScore maxXplayer minOplayer depth ["o","o","3",
                                                "x","x","o",
                                                "x","o","x"] `shouldBe` -98

    it "chooses a corner for the opening move" $ do
      moveIsInList (getAiMove emptyBoard xPlayer oPlayer) ["1","3","7","9"] `shouldBe` True

    it "chooses the center following an opening corner move" $ do
      getAiMove ["x","2","3",
                 "4","5","6",
                 "7","8","9"] xPlayer oPlayer `shouldBe` "5"

    it "chooses a corner following an opening center move" $ do
      moveIsInList (getAiMove ["1","2","3",
                               "4","o","6",
                               "7","8","9"] xPlayer oPlayer)
                   ["1","3","7","9"] `shouldBe` True

    it "chooses a win in one move" $ do
      getAiMove ["x","x","3",
                 "4","5","6",
                 "7","8","9"] xPlayer oPlayer `shouldBe` "3"

    it "block the opponent's win in one move" $ do
      getAiMove ["o","o","3",
                 "4","5","6",
                 "7","8","9"] xPlayer oPlayer `shouldBe` "3"


    it "blocks L setup" $ do
      moveIsInList (getAiMove ["o","2","3",
                               "4","x","6",
                               "7","o","9"] xPlayer oPlayer)
        ["4","7","6","9"] `shouldBe` True

    it "blocks edge setup" $ do
      getAiMove ["x","o","3",
                 "o","5","6",
                 "7","8","9"] xPlayer oPlayer `shouldBe` "5"

    it "blocks opposing corner setup" $ do
       moveIsInList (getAiMove ["o","2","3",
                                "4","x","6",
                                "7","8","o"] xPlayer oPlayer)
         ["2","4","6","8"] `shouldBe` True

    it "returns first available move" $ do
      firstAvailableMove ["1","2","3"] `shouldBe` "1"

moveIsInList n list = length (filter (== n) list) >= 1

