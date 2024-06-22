import Test.Tasty

import Test.Route qualified as Route
import Test.Timing qualified as Timing

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ Timing.tests
      , Route.tests
      ]
