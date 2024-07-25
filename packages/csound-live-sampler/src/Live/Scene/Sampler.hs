module Live.Scene.Sampler (
  Sampler (..),
  SamplerDeps (..),
  TrackId (..),
  newSampler,
  setTrack,
  module X,
) where

import Csound.Core
import Data.Boolean (false, true, (==*))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Live.Scene.Common (ChannelId (..))
import Live.Scene.Sampler.Audio
import Live.Scene.Sampler.Config as X
import Live.Scene.Sampler.Engine
import Live.Scene.Sampler.Metronome (MetronomeDeps (..), newMetronomeInstr, playMetronome)
import Live.Scene.Sampler.Playlist (
  Cursor (..),
  Playlist (..),
  TrackId (..),
  newPlaylist,
  nextPart,
  setTrack,
 )

data Sampler = Sampler
  { cursor :: Cursor
  , playExtraClip :: ColumnName -> ClipName -> SE ()
  , start :: SE ()
  , stop :: SE ()
  , getTrackIds :: [TrackId]
  , readBpm :: SE Sig
  , readTicks :: SE Sig
  , currentBeat :: SE Sig
  , readIsMainClipChange :: SE Sig
  , readClip :: SE Clip
  }

data SamplerDeps = SamplerDeps
  { writeChannel :: ChannelId -> Sig2 -> SE ()
  }

newSampler :: SamplerConfig ChannelId -> SamplerDeps -> SE Sampler
newSampler configUnordered deps = do
  audio <- setupAudio config (AudioDeps deps.writeChannel)
  playlist <- newPlaylist config (fmap (toSig . getInstrRefIdNum) audio.mainTrackInstrs)
  let
    getNextPart = do
      nextPart playlist.cursor
      getPart playlist
  engine <- newEngine getNextPart (getExtraClipColumnSize config)
  mapM_ (runMetronome deps engine.readTicks engine.currentBeat) config.metronome
  pure $
    Sampler
      { cursor = initSamplerCursor playlist engine
      , start = engine.start
      , stop = engine.stop
      , getTrackIds = allTrackIds config
      , playExtraClip = playExtraClipSt engine audio.extraClips
      , readBpm = engine.readBpm
      , readTicks = engine.readTicks
      , currentBeat = engine.currentBeat
      , readIsMainClipChange = engine.readIsMainClipChange
      , readClip = engine.readClip
      }
  where
    config = orderTracks configUnordered

orderTracks :: forall a. SamplerConfig a -> SamplerConfig a
orderTracks config =
  case config.playlist of
    Nothing -> config
    Just playlist -> config{tracks = order playlist config.tracks}
  where
    order :: [Text] -> [TrackConfig a] -> [TrackConfig a]
    order playlist tracks
      | not (null playlist) = (fmap fst $ List.sortOn snd $ map addOrder inPlaylistTracks) <> notInPlaylistTracks
      | otherwise = tracks
      where
        (inPlaylistTracks, notInPlaylistTracks) = List.partition (\track -> Map.member track.name orderMap) tracks

        orderMap :: Map Text Int
        orderMap = Map.fromList $ zip playlist [0 ..]

        maxOrder :: Int
        maxOrder = length playlist + 1

        addOrder :: TrackConfig a -> (TrackConfig a, Int)
        addOrder track =
          case Map.lookup track.name orderMap of
            Just priority -> (track, priority)
            Nothing -> (track, maxOrder)

playExtraClipSt :: Engine -> ExtraClips -> ColumnName -> ClipName -> SE ()
playExtraClipSt engine extraClips column clip =
  mapM_
    (uncurry engine.setExtraClipPart)
    ( (,)
        <$> findExtraColumn extraClips.columns column
        <*> findExtraPart extraClips.clips column clip
    )

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
            , measure = int $ maybe 4 fst $ clip.config.measure
            , trackIndex = int 0
            , partIndex = int 0
            , numOfParts = int 1
            }
      , track = toSig (getInstrRefIdNum clip.instr)
      }

-- | Converts beats to seconds
toAbsTime :: Float -> Float -> Float
toAbsTime bpm beats = 60 * beats / bpm

getExtraClipColumnSize :: SamplerConfig ChannelId -> ExtraClipSize
getExtraClipColumnSize config =
  maybe 0 (length . (.columns)) config.clips

allTrackIds :: SamplerConfig ChannelId -> [TrackId]
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

runMetronome :: SamplerDeps -> SE Sig -> SE Sig -> MetronomeConfig ChannelId -> SE ()
runMetronome deps readClicks currentBeat config = do
  metronomeInstr <- newMetronomeInstr (toMetronomeDeps deps) config
  clicks <- readClicks
  beat <- currentBeat
  when1 (clicks ==* 1) $
    whens
      [(beat ==* 0, playMetronome metronomeInstr true)]
      (playMetronome metronomeInstr false)
  where
    toMetronomeDeps SamplerDeps{..} = MetronomeDeps{..}
