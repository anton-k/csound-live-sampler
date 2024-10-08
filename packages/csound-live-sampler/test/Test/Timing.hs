module Test.Timing (
  tests,
) where

import Live.Scene.Sampler.Config
import Live.Scene.Sampler.Timing
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Timing"
    [ testCase "one clip" $ check1
    , testCase "two clips" $ check2
    , testCase "two clips with a gap" $ check3
    , testCase "3 clips with BPM change" check4
    ]

check1 :: Assertion
check1 =
  splitStem 0 [slot] @?= [clip]
  where
    slot =
      TimeSlot
        { bpm = 60
        , measure = Just (4, 4)
        , changeRate = Just 4
        , timeScale = Nothing
        , cues =
            [ Cue
                { start = Nothing
                , dur = 20
                , nextAction = Just PlayLoop
                }
            ]
        }

    clip =
      Clip
        { bpm = 60
        , changeRate = 4
        , start = 0
        , beatSize = 20
        , timeSize = 20
        , nextAction = PlayLoop
        , measure = 4
        , trackIndex = 0
        , partIndex = 0
        , numOfParts = 1
        }

check2 :: Assertion
check2 =
  splitStem 0 [slot] @?= [clip1, clip2]
  where
    slot =
      TimeSlot
        { bpm = 120
        , measure = Just (4, 4)
        , changeRate = Just 4
        , timeScale = Nothing
        , cues =
            [ Cue
                { start = Nothing
                , dur = 20
                , nextAction = Just PlayLoop
                }
            , Cue
                { start = Nothing
                , dur = 10
                , nextAction = Just PlayLoop
                }
            ]
        }

    clip1 =
      Clip
        { bpm = 120
        , changeRate = 4
        , start = 0
        , beatSize = 20
        , timeSize = 10
        , nextAction = PlayLoop
        , measure = 4
        , trackIndex = 0
        , partIndex = 0
        , numOfParts = 2
        }

    clip2 =
      Clip
        { bpm = 120
        , changeRate = 4
        , start = 10
        , beatSize = 10
        , timeSize = 5
        , nextAction = PlayLoop
        , measure = 4
        , trackIndex = 0
        , partIndex = 1
        , numOfParts = 2
        }

check3 :: Assertion
check3 =
  splitStem 0 [slot] @?= [clip1, clip2]
  where
    slot =
      TimeSlot
        { bpm = 120
        , measure = Just (4, 4)
        , changeRate = Just 4
        , timeScale = Nothing
        , cues =
            [ Cue
                { start = Just 2
                , dur = 20
                , nextAction = Just PlayLoop
                }
            , Cue
                { start = Just 4
                , dur = 10
                , nextAction = Just PlayLoop
                }
            ]
        }

    clip1 =
      Clip
        { bpm = 120
        , changeRate = 4
        , start = 1
        , beatSize = 20
        , timeSize = 10
        , nextAction = PlayLoop
        , measure = 4
        , trackIndex = 0
        , partIndex = 0
        , numOfParts = 2
        }

    clip2 =
      Clip
        { bpm = 120
        , changeRate = 4
        , start = 13
        , beatSize = 10
        , timeSize = 5
        , nextAction = PlayLoop
        , measure = 4
        , trackIndex = 0
        , partIndex = 1
        , numOfParts = 2
        }

check4 :: Assertion
check4 =
  splitStem 0 [slot1, slot2] @?= [clip1, clip2, clip3]
  where
    slot1 =
      TimeSlot
        { bpm = 120
        , measure = Just (4, 4)
        , changeRate = Just 4
        , timeScale = Nothing
        , cues =
            [ Cue
                { start = Just 2
                , dur = 20
                , nextAction = Just PlayLoop
                }
            , Cue
                { start = Just 4
                , dur = 10
                , nextAction = Just PlayLoop
                }
            ]
        }

    slot2 =
      TimeSlot
        { bpm = 30
        , measure = Just (4, 4)
        , changeRate = Just 2
        , timeScale = Nothing
        , cues =
            [ Cue
                { start = Just 2
                , dur = 20
                , nextAction = Just PlayLoop
                }
            ]
        }

    clip1 =
      Clip
        { bpm = 120
        , changeRate = 4
        , start = 1
        , beatSize = 20
        , timeSize = 10
        , nextAction = PlayLoop
        , measure = 4
        , trackIndex = 0
        , partIndex = 0
        , numOfParts = 3
        }

    clip2 =
      Clip
        { bpm = 120
        , changeRate = 4
        , start = 13
        , beatSize = 10
        , timeSize = 5
        , nextAction = PlayLoop
        , measure = 4
        , trackIndex = 0
        , partIndex = 1
        , numOfParts = 3
        }

    clip3 =
      Clip
        { bpm = 30
        , changeRate = 2
        , start = 22
        , beatSize = 20
        , timeSize = 40
        , nextAction = PlayLoop
        , measure = 4
        , trackIndex = 0
        , partIndex = 2
        , numOfParts = 3
        }
