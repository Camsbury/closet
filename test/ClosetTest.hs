--------------------------------------------------------------------------------
module ClosetTest where
--------------------------------------------------------------------------------
import Test.Prelude
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test_Closet

test_Closet :: TestTree
test_Closet
  = localOption (HedehogTestLimit 5)
  $ testGroup "closet tests" []
