module Test.Network.AWS.Flow
  ( tests
  ) where

import Network.AWS.Flow

import BasicPrelude
import Test.Tasty
import Test.Tasty.HUnit

assertUserError :: String -> IO a -> IO ()
assertUserError s action =
  handleJust check (const $ return ()) $ do
    void $ action
    assertFailure $ "missed user error: " ++ s where
      check e = guard $ userError s == e

testSelect :: TestTree
testSelect =
  testGroup "Select tests"
    [ testCase "No events" $
        assertUserError "No Next Event" $ runDecide undefined undefined [] select
    ]

tests :: TestTree
tests =
  testGroup "Flow tests"
    [ testSelect
    ]
