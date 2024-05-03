module Live.Scene.Sampler
  ( Sampler (..)
  , TrackId (..)
  , newSampler
  , setTrack
  , module X
  ) where

import Csound.Core
import Live.Scene.Gen as X
import Live.Scene.Sampler.Config as X
import Live.Scene.Sampler.Engine
import Live.Scene.Sampler.Playlist
  ( Cursor (..)
  , setTrack
  , Playlist (..)
  , newPlaylist
  , TrackId (..)
  , nextPart
  )
import Live.Scene.Sampler.Audio
import Data.Map.Strict qualified as Map

data Sampler = Sampler
  { audio :: Gen
  , cursor :: Cursor
  , playExtraClip :: ColumnName -> ClipName -> SE ()
  , start :: SE ()
  , stop :: SE ()
  , getTrackIds :: [TrackId]
  }

newSampler :: SamplerConfig -> SE Sampler
newSampler config = do
  audio <- setupAudio config
  playlist <- newPlaylist config (fmap (toSig . getInstrRefIdNum) audio.mainTrackInstrs)
  let
    getNextPart = do
      nextPart playlist.cursor
      getPart playlist
  engine <- newEngine getNextPart (getExtraClipColumnSize config)
  pure $ Sampler
    { audio = audio.gen
    , cursor = initSamplerCursor playlist engine
    , start = engine.start
    , stop = engine.stop
    , getTrackIds = allTrackIds config
    , playExtraClip = playExtraClipSt engine audio.extraClips
    }

playExtraClipSt :: Engine -> ExtraClips -> ColumnName -> ClipName -> SE ()
playExtraClipSt engine extraClips column clip =
  mapM_ (uncurry engine.setExtraClipPart) ((,)
    <$> findExtraColumn extraClips.columns column <*> findExtraPart extraClips.clips column clip)

findExtraColumn :: ColumnNameMap -> ColumnName -> Maybe ColumnId
findExtraColumn names column = int <$> Map.lookup column names

findExtraPart :: ClipMap -> ColumnName -> ClipName -> Maybe Part
findExtraPart clipMap columnName clipName = do
  column <- Map.lookup columnName clipMap
  clip <- Map.lookup clipName column
  pure $
    Part
      { clip =
          Clip
            { bpm = float clip.config.bpm
            , start = float $ maybe 0 (toAbsTime clip.config.bpm . fromIntegral) clip.config.start
            , changeRate = maybe 1 int clip.config.changeRate
            , beatSize = int clip.config.dur
            , timeSize = float $ toAbsTime clip.config.bpm (fromIntegral clip.config.dur)
            , nextAction = int $ maybe 0 fromEnum clip.config.nextAction
            }
      , track = toSig (getInstrRefIdNum clip.instr)
      }

-- | Converts beats to seconds
toAbsTime :: Float -> Float -> Float
toAbsTime bpm beats = 60 * beats / bpm

getExtraClipColumnSize :: SamplerConfig -> ExtraClipSize
getExtraClipColumnSize config =
  maybe 0 (length . (.columns)) config.clips

allTrackIds :: SamplerConfig -> [TrackId]
allTrackIds config =
  TrackId . int <$> [0 .. length config.tracks - 1]

initSamplerCursor :: Playlist -> Engine -> Cursor
initSamplerCursor playlist engine =
  Cursor
    { modifyTrack = \f -> do
        playlist.cursor.modifyTrack f
        part <- playlist.getPart
        engine.setTrackPart part

    , modifyPart = \f -> do
        playlist.cursor.modifyPart f
        part <- playlist.getPart
        engine.setTrackPart part
    }
