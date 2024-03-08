module Live.Scene.Sampler
  ( Sampler (..)
  , newSampler
  , module X
  ) where

import Data.Bifunctor
import Csound.Core
import Live.Scene.Gen as X
import Live.Scene.Sampler.Config as X
import Safe (atMay)
import Data.Function qualified as Function
import Data.List qualified as List
import Data.Maybe
import Data.Boolean
import Data.Map.Strict qualified as Map

newtype TrackId = TrackId Int

data Sampler = Sampler
  { audio :: Gen
  , setTrack :: TrackId -> SE ()
  , getTrackIds :: [TrackId]
  }

newSampler :: SamplerConfig -> SE Sampler
newSampler config = do
  st <- initSt config
  pure $ Sampler
    { audio =
        emptyGen
          { read = readSt st
          , outputs = ChannelId <$> [0 .. (length st.channels - 1)]
          }
    , setTrack = setTrackSt st
    , getTrackIds = TrackId <$> [0 .. (length config.tracks - 1)]
    }
  where

-- * internal state

data St = St
  { currentTrack :: CurrentTrack
  , channels :: [Channel]
  , tracks :: [Track]
  }

data Track = Track
  { instr :: InstrRef ()
  }

data Channel = Channel { audio :: Ref Sig2 }

data CurrentTrack = CurrentTrack (Ref D)

setCurrentTrack :: CurrentTrack -> InstrRef () -> SE ()
setCurrentTrack (CurrentTrack ref) instrRef =
  case getInstrRefId instrRef of
    Right n -> writeRef ref n
    Left _ -> pure ()

getCurrentTrack :: CurrentTrack -> SE D
getCurrentTrack (CurrentTrack ref) =
  readRef ref

initSt :: SamplerConfig -> SE St
initSt config = do
  currentTrack <- CurrentTrack <$> (newCtrlRef (-1))
  channels <- mapM (fmap Channel . newRef . const 0) [0 .. maxChannel]
  tracks <- mapM (trackInstr channels) config.tracks
  pure St {..}
  where
    maxChannel = maximum $ (0 : ) $ do
      track <- config.tracks
      stem <- track.stems
      pure $ stem.channel

readSt :: St -> ChannelId -> SE Sig2
readSt st channelId =
  maybe (pure 0) (readRef . (.audio)) (getChannel st channelId)

getChannel :: St -> ChannelId -> Maybe Channel
getChannel st (ChannelId n) = st.channels `atMay` n

getTrack :: St -> TrackId -> Maybe Track
getTrack st (TrackId n) = st.tracks `atMay` n

withTrack :: St -> TrackId -> (Track -> SE ()) -> SE ()
withTrack st trackId f = mapM_ f (getTrack st trackId)

setTrackSt :: St -> TrackId -> SE ()
setTrackSt st trackId =
  withTrack st trackId $ \track -> do
    currentTrack <- getCurrentTrack st.currentTrack
    when1 (currentTrack /=* getInstrRefIdNum track.instr) $ do
      setCurrentTrack st.currentTrack track.instr
      when1 (currentTrack >* 0) $
        turnoff2_i (instrRefFromNum @() currentTrack) 0 0.05
      play track.instr [Note 0 (-1) ()]

trackInstr :: [Channel] -> TrackConfig -> SE Track
trackInstr channels track =
  fmap Track $ newProc $ \() -> playTrack channels track

playTrack :: [Channel] -> TrackConfig -> SE ()
playTrack channels track =
  mapM_ playStemGroup (groupStemsByChannels channels track.stems)
  where
    playStemGroup :: StemGroup -> SE ()
    playStemGroup group =
      writeRef group.channel.audio audio
      where
        audio = withGain track.gain $ sum $ fmap playStem group.stems

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

    size = length channels

    emptyChan chan = StemGroup chan []

playStem :: StemConfig -> Sig2
playStem stem =
  withGain stem.gain $ mul (maybe 1 float stem.volume) (loopWav (fromString stem.file) 1)

withGain :: Maybe Float -> Sig2 -> Sig2
withGain mValue audio =
  case mValue of
    Nothing -> audio
    Just value -> mul (float value) audio

