{-# Language
  DuplicateRecordFields,
  OverloadedStrings,
  OverloadedRecordDot
#-}
import Csound.Core
import Live.Scene.Sampler.Config
import Live.Scene.Sampler.Playlist
import Data.String

main =
  dacBy setTrace $ do
  -- writeCsdBy (setMa <> setDac) "tmp.csd" $ do
    playlist <- newPlaylist config instrIds
    sequence_ $ replicate 8 (makeStep playlist)
  where
    makeStep playlist = do
      part <- playlist.getPart
      printPart part
      nextPart playlist

    printPart part = prints "Part: %d %d %d %d\n" (part.track, part.clip.bpm, part.clip.start, part.clip.timeSize)

    instrIds = instrRefFromNum <$> [1,2,3]

    config =
      SamplerConfig
        { tracks = zipWith toTrack [0..] [track1, track2]
        , dir = Nothing
        }

    track1 = [slot1, slot2]
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

    track2 = [slot]
      where
        slot = TimeSlot
          { bpm = 60
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


toTrack :: Int -> [TimeSlot] -> TrackConfig
toTrack trackId slots = TrackConfig
  { dir = Nothing
  , name = fromString $ "Track" <> show trackId
  , stems = []
  , slots
  , gain = Nothing
  }


