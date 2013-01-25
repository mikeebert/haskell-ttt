import Test.Hspec
import qualified BoardTest

main :: IO ()
main = hspec $ do
  describe "Board Specs" BoardTest.spec
