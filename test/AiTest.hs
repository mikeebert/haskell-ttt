module AiTest where

import Test.Hspec
import Test.QuickCheck
import Player
import Ai

main :: IO ()
main = hspec spec

wonXBoard = ["x","x","x","4","5","6","7","8","9"]
wonOBoard = ["1","2","3","4","5","6","o","o","o"]

drawGameBoard = ["x","o","x",
                 "x","o","x",
                 "o","x","o"]

minOplayer = Player {piece = "o", strategy = "min", startingScore = 5}
maxXplayer = Player {piece = "x", strategy = "max", startingScore = -5}

spec :: Spec
spec = do
  describe "Ai" $ do
    it "returns a dumb move" $ do
      listIncludes (getDumbMove ["1","2","3"]) ["1","2","3"] `shouldBe` True

    it "returns the value of a won board in 1 move" $ do
      value wonXBoard maxXplayer minOplayer 1 `shouldBe` 99

    it "returns the value of a lost board in 1 move" $ do
      value wonOBoard maxXplayer minOplayer 1 `shouldBe` -99

    it "returns 0 as the value for a draw game" $ do
      value drawGameBoard maxXplayer minOplayer 9 `shouldBe` 0

    it "returns -1 as the value for a game in progress" $ do
      value ["1","2","3","4","5","7","8","9"] maxXplayer minOplayer 0 `shouldBe` -1

    it "scores a won board for the max player" $ do
      minimaxScore maxXplayer minOplayer 5 wonXBoard `shouldBe` 95

    it "returns move with highest score" $ do
      bestMove [(95,"1"), (-99,"2"), (-1,"3"), (96,"4"), (-1,"5")] `shouldBe` "4" 

listIncludes n list = length (filter (== n) list) >= 1

