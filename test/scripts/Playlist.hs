{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Csound.Core
import Data.Boolean (true, (/=*), (==*))
import Data.String
import Live.Scene.Sampler.Config
import Live.Scene.Sampler.Playlist

main =
  -- dacBy setTrace $ do
  writeCsdBy (setMa <> setDac) "tmp.csd" $ do
    playlist <- newPlaylist config instrIds
    instr1 <- newProc $ \(n :: D) -> do
      whens
        [ (n ==* 1, block "Iterate parts" (iterateParts 2) playlist)
        , (n ==* 2, block "Iterate tracks" iterateTracks playlist)
        ]
        (pure ())
    -- block "Iterate tracks with shift" iterateTracksWithShift playlist
    play instr1 [Note 0 0.5 1]
    play instr1 [Note 1 0.5 2]
  where
    block name proc playlist = do
      newline
      title name
      proc playlist

    makeStep playlist = do
      part <- playlist.getPart
      printPart part
      nextPart playlist.cursor

    makeTrackStep playlist = do
      part <- playlist.getPart
      printPart part
      nextTrack playlist.cursor

    iterateParts n playlist =
      sequence_ $ replicate n (makeStep playlist)

    iterateTracks playlist = do
      setPart playlist.cursor (TrackId 0) (ClipId 0)
      sequence_ $ replicate 4 (makeTrackStep playlist)

    iterateTracksWithShift playlist = do
      setPart playlist.cursor (TrackId 0) (ClipId 0)
      nextPart playlist.cursor
      sequence_ $ replicate 4 (makeTrackStep playlist)

    newline = prints "\n" ()

    title :: Str -> SE ()
    title name = prints "%s\n" name

    printPart part = prints "part: { instr: %d, bpm: %d, start: %d, dur: %d }\n" (part.track, part.clip.bpm, part.clip.start, part.clip.timeSize)

    instrIds = instrRefFromNum <$> [1, 2, 3]

    config =
      SamplerConfig
        { tracks = zipWith toTrack [0 ..] [track1, track2, track3]
        , dir = Nothing
        }

    track1 = [slot1, slot2]
      where
        slot1 =
          TimeSlot
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

        slot2 =
          TimeSlot
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
        slot =
          TimeSlot
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
        slot =
          TimeSlot
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
toTrack trackId slots =
  TrackConfig
    { dir = Nothing
    , name = fromString $ "Track" <> show trackId
    , stems = []
    , slots
    , gain = Nothing
    }
