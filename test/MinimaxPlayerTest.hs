module MinimaxPlayerTest where

import Test.Hspec
import Test.QuickCheck
import MinimaxPlayer

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MinimaxPlayer" $ do
    it "sets up Max player" $ do
      let maxPlayer = setupMax "x"
      strategy maxPlayer `shouldBe` "max"
      symbol maxPlayer `shouldBe` "x"

    it "sets up Min player" $ do
      let minPlayer = setupMin "x"
      strategy minPlayer `shouldBe` "min"
      symbol minPlayer `shouldBe` "x"
