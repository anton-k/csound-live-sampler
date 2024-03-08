import Test.Tasty

import Test.Timing qualified as Timing

main :: IO ()
main = defaultMain Timing.tests
