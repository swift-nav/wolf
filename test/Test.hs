import           BasicPrelude
import qualified Test.Network.AWS.Flow as Flow
import           Test.Tasty

tests :: TestTree
tests =
  testGroup "Tests"
    [ Flow.tests
    ]

main :: IO ()
main = defaultMain tests
