import Test.Hspec
import qualified BoardTest
import qualified AiTest
import qualified ConsoleRunnerTest

main :: IO ()
main = hspec $ do
  describe "Board Tests" BoardTest.spec
  describe "Ai Tests" AiTest.spec
  describe "ConsoleRunner Tests" ConsoleRunnerTest.spec
