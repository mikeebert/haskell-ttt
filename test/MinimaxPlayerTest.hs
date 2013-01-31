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
      strategy (setupMax "x") `shouldBe` "max"
      symbol (setupMax "x") `shouldBe` "x"

    it "sets up Min player" $ do
      strategy (setupMin "x") `shouldBe` "min"
      symbol (setupMin "x") `shouldBe` "x"
