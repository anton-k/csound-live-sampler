module Test.Timing
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Live.Scene.Sampler.Config
import Live.Scene.Sampler.Timing

tests :: TestTree
tests = testGroup "Timing"
  [ testCase "one clip" $ check1
  , testCase "two clips" $ check2
  , testCase "two clips with a gap" $ check3
  , testCase "3 clips with BPM change" check4
  ]

check1 :: Assertion
check1 =
  splitStem [slot] @?= [clip]
  where
    slot = TimeSlot
      { bpm = 60
      , measure = Just (4, 4)
      , changeRate = Just 4
      , cues =
          [ Cue
              { start = Nothing
              , dur = 20
              }
          ]
      }

    clip =
      Clip
        { bpm = 60
        , changeRate = 4
        , start = 0
        , size = 20
        }

check2 :: Assertion
check2 =
  splitStem [slot] @?= [clip1, clip2]
  where
    slot = TimeSlot
      { bpm = 120
      , measure = Just (4, 4)
      , changeRate = Just 4
      , cues =
          [ Cue
              { start = Nothing
              , dur = 20
              }
          , Cue
              { start = Nothing
              , dur = 10
              }
          ]
      }

    clip1 =
      Clip
        { bpm = 120
        , changeRate = 4
        , start = 0
        , size = 20
        }

    clip2 =
      Clip
        { bpm = 120
        , changeRate = 4
        , start = 10
        , size = 10
        }


check3 :: Assertion
check3 =
  splitStem [slot] @?= [clip1, clip2]
  where
    slot = TimeSlot
      { bpm = 120
      , measure = Just (4, 4)
      , changeRate = Just 4
      , cues =
          [ Cue
              { start = Just 2
              , dur = 20
              }
          , Cue
              { start = Just 4
              , dur = 10
              }
          ]
      }

    clip1 =
      Clip
        { bpm = 120
        , changeRate = 4
        , start = 1
        , size = 20
        }

    clip2 =
      Clip
        { bpm = 120
        , changeRate = 4
        , start = 13
        , size = 10
        }


check4 :: Assertion
check4 =
  splitStem [slot1, slot2] @?= [clip1, clip2, clip3]
  where
    slot1 = TimeSlot
      { bpm = 120
      , measure = Just (4, 4)
      , changeRate = Just 4
      , cues =
          [ Cue
              { start = Just 2
              , dur = 20
              }
          , Cue
              { start = Just 4
              , dur = 10
              }
          ]
      }

    slot2 = TimeSlot
      { bpm = 30
      , measure = Just (4, 4)
      , changeRate = Just 2
      , cues =
          [ Cue
              { start = Just 2
              , dur = 20
              }
          ]
      }

    clip1 =
      Clip
        { bpm = 120
        , changeRate = 4
        , start = 1
        , size = 20
        }

    clip2 =
      Clip
        { bpm = 120
        , changeRate = 4
        , start = 13
        , size = 10
        }

    clip3 =
      Clip
        { bpm = 30
        , changeRate = 2
        , start = 22
        , size = 20
        }
