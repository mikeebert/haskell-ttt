import Test.Hspec
import qualified BoardSpec

main :: IO ()
main = hspec $ do
  describe "Board" BoardSpec.spec
