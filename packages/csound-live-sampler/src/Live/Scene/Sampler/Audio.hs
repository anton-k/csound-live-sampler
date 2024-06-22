{-| audio-rate instruments for sampler
and outupt to packaging as audio unit.
-}
module Live.Scene.Sampler.Audio (
  Audio (..),
  ExtraClips (..),
  ColumnNameMap,
  ClipMap,
  ClipInfo (..),
  setupAudio,
  AudioDeps (..),
) where

import Control.Applicative ((<|>))
import Csound.Core
import Data.Function qualified as Function
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Live.Scene.Common (ChannelId (..))
import Live.Scene.Sampler.Config

type ClipInstr = InstrRef D

data Audio = Audio
  { mainTrackInstrs :: [ClipInstr]
  , extraClips :: ExtraClips
  }

data AudioDeps = AudioDeps
  { writeChannel :: ChannelId -> Sig2 -> SE ()
  }

setupAudio :: SamplerConfig ChannelId -> AudioDeps -> SE Audio
setupAudio config deps = do
  instrIds <- newTrackInstrs config deps
  clips <- newExtraClips deps config
  pure $
    Audio
      { mainTrackInstrs = instrIds
      , extraClips = clips
      }

-- main tracks

newTrackInstrs :: SamplerConfig ChannelId -> AudioDeps -> SE [ClipInstr]
newTrackInstrs config deps =
  mapM (trackInstr deps) config.tracks

trackInstr :: AudioDeps -> TrackConfig ChannelId -> SE ClipInstr
trackInstr deps track =
  newProc $ \skipStartTime -> playTrack deps skipStartTime track

playTrack :: AudioDeps -> D -> TrackConfig ChannelId -> SE ()
playTrack deps skipStartTime track =
  mapM_ playStemGroup (groupStemsByChannels track.stems)
  where
    playStemGroup :: StemGroup -> SE ()
    playStemGroup group =
      deps.writeChannel group.channel audio
      where
        audio = withGain track.gain $ sum $ fmap (playStem skipStartTime) group.stems

-- | Group of stems that belong to the same channel
data StemGroup = StemGroup
  { channel :: ChannelId
  , stems :: [StemConfig ChannelId]
  }

groupStemsByChannels :: [StemConfig ChannelId] -> [StemGroup]
groupStemsByChannels stems =
  mapMaybe fromGroup $ groupOn (.channel) $ List.sortOn (.channel) stems
  where
    groupOn f = List.groupBy ((==) `Function.on` f)

    fromGroup :: [StemConfig ChannelId] -> Maybe StemGroup
    fromGroup = \case
      [] -> Nothing
      x : xs -> Just (StemGroup x.channel (x : xs))

playStem :: D -> StemConfig ChannelId -> Sig2
playStem skipStartTime stem =
  withGain stem.gain $ mul (maybe 1 float stem.volume) (diskin2 (fromString stem.file) `withDs` [1, skipStartTime])

withGain :: Maybe Float -> Sig2 -> Sig2
withGain mValue audio =
  case mValue of
    Nothing -> audio
    Just value -> mul (float value) audio

-- extra clips

data ExtraClips = ExtraClips
  { columns :: ColumnNameMap
  , clips :: ClipMap
  }

newExtraClips :: AudioDeps -> SamplerConfig ChannelId -> SE ExtraClips
newExtraClips deps config = do
  clips <- maybe (pure mempty) (newClipMap deps) config.clips
  pure $
    ExtraClips
      { columns = initColumnNames config
      , clips
      }

type ColumnNameMap = Map ColumnName Int

type ClipMap = Map ColumnName (Map ClipName ClipInfo)

data ClipInfo = ClipInfo
  { config :: ClipConfig ChannelId
  , instr :: InstrRef (D, D)
  }

initColumnNames :: SamplerConfig ChannelId -> ColumnNameMap
initColumnNames config =
  maybe mempty (toMap . (.columns)) config.clips
  where
    toMap columns =
      Map.fromList $ zipWith (\column index -> (column.name, index)) columns [0 ..]

newClipMap :: AudioDeps -> ClipsConfig ChannelId -> SE ClipMap
newClipMap deps config =
  Map.fromList <$> mapM fromColumn config.columns
  where
    fromColumn :: ClipColumnConfig ChannelId -> SE (ColumnName, Map ClipName ClipInfo)
    fromColumn column =
      (column.name,) . Map.fromList <$> mapM (fromClip column) column.clips

    fromClip :: ClipColumnConfig ChannelId -> ClipConfig ChannelId -> SE (ClipName, ClipInfo)
    fromClip column clip =
      (clip.name,) <$> toClipInfo column clip

    toClipInfo :: ClipColumnConfig ChannelId -> ClipConfig ChannelId -> SE ClipInfo
    toClipInfo column clip =
      ClipInfo clip <$> toExtraClipInstrRef deps column clip

toExtraClipInstrRef :: AudioDeps -> ClipColumnConfig ChannelId -> ClipConfig ChannelId -> SE (InstrRef (D, D))
toExtraClipInstrRef deps column config =
  newProc $ toExtraClipInstrBody writeOut config
  where
    channelId = fromMaybe (ChannelId 0) $ config.channel <|> column.channel

    writeOut ins = deps.writeChannel channelId (withGain column.gain ins)

toExtraClipInstrBody :: (Sig2 -> SE ()) -> ClipConfig ChannelId -> (D, D) -> SE ()
toExtraClipInstrBody writeOut config =
  case fromMaybe Diskin config.mode of
    Diskin -> diskinInstr
    Flooper -> flooperInstr
  where
    diskinInstr :: (D, D) -> SE ()
    diskinInstr (mainBpm, skipStartTime) = do
      writeOut $
        withGain config.gain $
          (diskin2 (fromString config.file) `withDs` [mainBpm / float config.bpm, skipStartTime])

    flooperInstr :: (D, D) -> SE ()
    flooperInstr (mainBpm, skipStartTime) =
      writeOut $
        withGain config.gain $
          flooper 1 (toSig icps) skipStartTime (skipStartTime + idur / icps) 0.05 itab
      where
        icps = mainBpm / float config.bpm
        itab = wavs config.file 0 WavAll
