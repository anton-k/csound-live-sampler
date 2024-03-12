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
    block "Iterate parts" iterateParts playlist
    block "Iterate tracks" iterateTracks playlist
    block "Iterate tracks with shift" iterateTracksWithShift playlist
  where
    block name proc playlist = do
      newline
      title name
      proc playlist

    makeStep playlist = do
      part <- playlist.getPart
      printPart part
      nextPart playlist

    makeTrackStep playlist = do
      part <- playlist.getPart
      printPart part
      nextTrack playlist

    iterateParts playlist =
      sequence_ $ replicate 8 (makeStep playlist)

    iterateTracks playlist = do
      setPart playlist (TrackId 0) (ClipId 0)
      sequence_ $ replicate 4 (makeTrackStep playlist)

    iterateTracksWithShift playlist = do
      setPart playlist (TrackId 0) (ClipId 0)
      nextPart playlist
      sequence_ $ replicate 4 (makeTrackStep playlist)

    newline = prints "\n" ()

    title :: Str -> SE ()
    title name = prints "%s\n" name

    printPart part = prints "part: { instr: %d, bpm: %d, start: %d, dur: %d }\n" (part.track, part.clip.bpm, part.clip.start, part.clip.timeSize)

    instrIds = instrRefFromNum <$> [1,2,3]

    config =
      SamplerConfig
        { tracks = zipWith toTrack [0..] [track1, track2, track3]
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

    track3 = [slot]
      where
        slot = TimeSlot
          { bpm = 140
          , measure = Just (4, 4)
          , changeRate = Just 4
          , cues =
              [ Cue
                  { start = Nothing
                  , dur = 20
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
