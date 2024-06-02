import Test.Tasty

import Test.Timing qualified as Timing
import Test.Route qualified as Route

main :: IO ()
main = defaultMain $
  testGroup "tests"
    [ Timing.tests
    , Route.tests
    ]
