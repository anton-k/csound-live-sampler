-- | audio-rate instruments for sampler
-- and outupt to packaging as audio unit.
module Live.Scene.Sampler.Audio
  ( setupAudio
  ) where

import Csound.Core
import Live.Scene.Sampler.Config
import Live.Scene.Gen
import Safe (atMay)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Function qualified as Function
import Data.Bifunctor
import Data.Maybe

type ClipInstr = InstrRef D

data Channel = Channel { audio :: Ref Sig2 }

setupAudio :: SamplerConfig -> SE (Gen, [ClipInstr])
setupAudio config = do
  channels <- setupOutputChannels config
  instrIds <- newTrackInstrs config channels
  pure (setupAudioGen channels, instrIds)

setupAudioGen :: [Channel] -> Gen
setupAudioGen channels =
  emptyGen
    { read = readChannel
    , outputs = ChannelId <$> [0 .. (length channels - 1)]
    }
  where
    readChannel :: ChannelId -> SE Sig2
    readChannel channelId =
      maybe (pure 0) (readRef . (.audio)) (getChannel channelId)

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
      writeRef group.channel.audio audio
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


