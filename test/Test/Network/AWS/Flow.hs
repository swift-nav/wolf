module Test.Network.AWS.Flow
  ( tests
  ) where

import Network.AWS.Flow
import Network.AWS.Flow.Prelude hiding ( catchIOError )

import Control.Monad.Catch
import Network.AWS.SWF
import Test.Tasty
import Test.Tasty.HUnit

testSelect :: TestTree
testSelect =
  testCase "First test..." $ do
    r <- runDecide undefined undefined [] select
    r @?= []

tests :: TestTree
tests =
  testGroup "Flow tests"
    [ testSelect
    ]
