-- | audio-rate instruments for sampler
-- and outupt to packaging as audio unit.
module Live.Scene.Sampler.Audio
  ( Audio (..)
  , ExtraClips (..)
  , ColumnNameMap
  , ClipMap
  , ClipInfo (..)
  , setupAudio
  ) where

import Control.Applicative ((<|>))
import Csound.Core
import Live.Scene.Sampler.Config
import Live.Scene.Gen
import Safe (atMay)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Function qualified as Function
import Data.Bifunctor
import Data.Maybe

type ClipInstr = InstrRef D

data Channel = Channel { audio :: Ref Sig2 }

data Audio = Audio
  { gen :: Gen
  , mainTrackInstrs :: [ClipInstr]
  , extraClips :: ExtraClips
  }

setupAudio :: SamplerConfig -> SE Audio
setupAudio config = do
  channels <- setupOutputChannels config
  instrIds <- newTrackInstrs config channels
  clips <- newExtraClips channels config
  pure $
    Audio
      { gen = setupAudioGen channels
      , mainTrackInstrs = instrIds
      , extraClips = clips
      }

-- main tracks

setupAudioGen :: [Channel] -> Gen
setupAudioGen channels =
  emptyGen
    { read = readChannel
    , outputs = ChannelId <$> [0 .. (length channels - 1)]
    }
  where
    readChannel :: ChannelId -> SE Sig2
    readChannel channelId =
      maybe (pure 0) (readAndClear . (.audio)) (getChannel channelId)

    readAndClear ref = do
      res <- readRef ref
      writeRef ref 0
      pure res

    getChannel :: ChannelId -> Maybe Channel
    getChannel (ChannelId n) = channels `atMay` n

setupOutputChannels :: SamplerConfig -> SE [Channel]
setupOutputChannels config =
  mapM (const initChannel) [0 .. maxChannel]
  where
    maxChannel = maximum $ (0 : ) $ do
      track <- config.tracks
      stem <- track.stems
      pure $ stem.channel

    initChannel = Channel <$> newRef 0

newTrackInstrs :: SamplerConfig -> [Channel] -> SE [ClipInstr]
newTrackInstrs config channels =
  mapM (trackInstr channels) config.tracks

trackInstr :: [Channel] -> TrackConfig -> SE ClipInstr
trackInstr channels track =
  newProc $ \skipStartTime -> playTrack channels skipStartTime track

playTrack :: [Channel] -> D -> TrackConfig -> SE ()
playTrack channels skipStartTime track =
  mapM_ playStemGroup (groupStemsByChannels channels track.stems)
  where
    playStemGroup :: StemGroup -> SE ()
    playStemGroup group =
      modifyRef group.channel.audio (+ audio)
      where
        audio = withGain track.gain $ sum $ fmap (playStem skipStartTime) group.stems

-- | Group of stems that belong to the same channel
data StemGroup = StemGroup
  { channel :: Channel
  , stems :: [StemConfig]
  }

groupStemsByChannels :: [Channel] -> [StemConfig] -> [StemGroup]
groupStemsByChannels channels stems = fillMissing
  where
    groupOn f = List.groupBy ((==) `Function.on` f)

    fromGroup = \case
      [] -> Nothing
      x:xs ->
        let n = x.channel - 1
        in  fmap (\chan -> (ChannelId n, StemGroup chan (x:xs))) $ channels `atMay` n

    fillMissing = fmap (\(chanId, chan) -> fromMaybe (emptyChan chan) $ Map.lookup chanId stemMap) indexes

    stemMap = Map.fromList $ mapMaybe fromGroup $ groupOn (.channel) $ List.sortOn (.channel) stems

    indexes = fmap (first ChannelId) $ zip [0..] channels

    emptyChan chan = StemGroup chan []

playStem :: D -> StemConfig -> Sig2
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


newExtraClips :: [Channel] -> SamplerConfig -> SE ExtraClips
newExtraClips channels config = do
  clips <- maybe (pure mempty) (newClipMap channels) config.clips
  pure $ ExtraClips
    { columns = initColumnNames config
    , clips
    }

type ColumnNameMap = Map ColumnName Int

type ClipMap = Map ColumnName (Map ClipName ClipInfo)

data ClipInfo = ClipInfo
  { config :: ClipConfig
  , instr :: InstrRef (D, D)
  }

initColumnNames :: SamplerConfig -> ColumnNameMap
initColumnNames config =
  maybe mempty (toMap . (.columns)) config.clips
  where
    toMap columns =
      Map.fromList $ zipWith (\column index -> (column.name, index)) columns [0..]

newClipMap :: [Channel] -> ClipsConfig -> SE ClipMap
newClipMap channels config =
  Map.fromList <$> mapM fromColumn config.columns
  where
    fromColumn :: ClipColumnConfig -> SE (ColumnName, Map ClipName ClipInfo)
    fromColumn column =
      (column.name, ) . Map.fromList <$> mapM (fromClip column) column.clips

    fromClip :: ClipColumnConfig -> ClipConfig -> SE (ClipName, ClipInfo)
    fromClip column clip =
      (clip.name, ) <$> toClipInfo column clip

    toClipInfo :: ClipColumnConfig -> ClipConfig -> SE ClipInfo
    toClipInfo column clip =
      ClipInfo clip <$> toExtraClipInstrRef channels column clip

toExtraClipInstrRef :: [Channel] -> ClipColumnConfig -> ClipConfig -> SE (InstrRef (D, D))
toExtraClipInstrRef channels column config =
  newProc $ toExtraClipInstrBody writeOut config
  where
    channelId = maybe 0 (\x -> x - 1) $ config.channel <|> column.channel

    writeOut = maybe (const $ pure ()) (\channel ins -> modifyRef channel.audio (+ withGain column.gain ins)) (atMay channels channelId)

toExtraClipInstrBody :: (Sig2 -> SE ()) -> ClipConfig -> (D, D) -> SE ()
toExtraClipInstrBody writeOut config =
  case fromMaybe Diskin config.mode of
    Diskin -> diskinInstr
    Flooper -> flooperInstr
    where
      diskinInstr :: (D, D) -> SE ()
      diskinInstr (mainBpm, skipStartTime) = do
        writeOut $ withGain config.gain $
          (diskin2 (fromString config.file) `withDs` [mainBpm / float config.bpm, skipStartTime])

      flooperInstr :: (D, D) -> SE ()
      flooperInstr (mainBpm, skipStartTime) =
        writeOut $ withGain config.gain $
          flooper 1 (toSig icps) skipStartTime (skipStartTime + idur / icps) 0.05 itab
        where
          icps = mainBpm / float config.bpm
          itab = wavs config.file 0 WavAll

