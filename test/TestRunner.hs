import Test.Hspec
import qualified BoardTest
import qualified AiTest

main :: IO ()
main = hspec $ do
  describe "Board Tests" BoardTest.spec
  describe "Ai Tests" AiTest.spec
