import Test.Hspec
import qualified BoardTest
import qualified ConsoleRunnerTest
import qualified PlayerTest
import qualified MinimaxPlayerTest
import qualified AiTest

main :: IO ()
main = hspec $ do
  describe "ConsoleRunner Tests" ConsoleRunnerTest.spec
  describe "Board Tests" BoardTest.spec
  describe "Player Tests" PlayerTest.spec
  describe "MinimaxPlayer Tests" MinimaxPlayerTest.spec
  describe "Ai Tests" AiTest.spec
