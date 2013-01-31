module PlayerTest where

import Test.Hspec
import Test.QuickCheck
import Player

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Player" $ do
    it "sets up a player" $ do
      let testPlayer = setupPlayer "x" "computer"
      piece testPlayer `shouldBe` "x"
      kind testPlayer `shouldBe` "computer"

    it "tests for isComputer player" $ do
      let testComputerPlayer = setupPlayer "x" "computer"
          testHumanPlayer = setupPlayer "x" "human"
      isComputer testComputerPlayer `shouldBe` True
      isComputer testHumanPlayer `shouldBe` False

    it "tests for isHuman player" $ do
      let testComputerPlayer = setupPlayer "x" "computer"
          testHumanPlayer = setupPlayer "x" "human"
      isHuman testComputerPlayer `shouldBe` False
      isHuman testHumanPlayer `shouldBe` True
