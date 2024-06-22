module Live.Config (
  Config (..),
  MixerConfig (..),
  SamplerConfig (..),
  ControllerConfig (..),
  AudioConfig (..),
  readConfig,
  convertConfig,
  inlineConfig,
) where

import Csound.Core hiding (Config)
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Yaml qualified as Yaml
import Live.Config.Types
import Live.Config.Validate
import Live.Scene.AudioCard.Config
import Live.Scene.Common
import Live.Scene.Midi.Config
import Live.Scene.Mixer.Config
import Live.Scene.Sampler.Config
import Live.Scene.Sampler.Config qualified as ClipColumnConfig (ClipColumnConfig (..))
import Live.Scene.Sampler.Config qualified as ClipConfig (ClipConfig (..))
import Live.Scene.Sampler.Config qualified as SamplerConfig (SamplerConfig (..))
import Live.Scene.Sampler.Config qualified as StemConfig (StemConfig (..))
import System.FilePath

readConfig :: FilePath -> IO (Either Text Config)
readConfig file =
  fmap absPath <$> do
    config <- Yaml.decodeFileThrow file
    checkConfig config
  where
    absPath config = config{sampler = appendAbsPath config.sampler}

appendAbsPath :: SamplerConfig a -> SamplerConfig a
appendAbsPath = appendStemAbsPath . appendClipAbsPath

-- | makes path absolute for stems
appendStemAbsPath :: SamplerConfig a -> SamplerConfig a
appendStemAbsPath config =
  config{tracks = fmap (appendTrack $ appendPath config.dir) config.tracks}
  where
    appendTrack addRoot track =
      track{stems = fmap (appendStem trackDir) track.stems}
      where
        trackDir = addRoot . appendPath track.dir

    appendStem :: (FilePath -> FilePath) -> StemConfig a -> StemConfig a
    appendStem add stem =
      stem{StemConfig.file = add stem.file}

appendClipAbsPath :: SamplerConfig a -> SamplerConfig a
appendClipAbsPath config =
  config{SamplerConfig.clips = fmap (go $ appendPath config.dir) config.clips}
  where
    go :: (FilePath -> FilePath) -> ClipsConfig a -> ClipsConfig a
    go add x =
      x{columns = fmap (appendColumn (add . appendPath x.dir)) x.columns}

    appendColumn :: (FilePath -> FilePath) -> ClipColumnConfig a -> ClipColumnConfig a
    appendColumn add column =
      column{ClipColumnConfig.clips = fmap (appendClip (add . appendPath column.dir)) column.clips}

    appendClip :: (FilePath -> FilePath) -> ClipConfig a -> ClipConfig a
    appendClip add clip =
      clip{ClipConfig.file = add clip.file}

appendPath :: Maybe FilePath -> FilePath -> FilePath
appendPath mPrefix = maybe id (</>) mPrefix

-- * convert configs

convertConfig ::
  Config ->
  ( AudioConfig ChannelId
  , SamplerConfig ChannelId
  , MixerConfig ChannelId
  , MidiControllerConfig AudioInputId ChannelId Int
  )
convertConfig config = (audioConfig, samplerConfig, mixerConfig, midiConfig)
  where
    mixerConfig = fmap convertChannel config.mixer
    samplerConfig = fmap convertChannel config.sampler
    midiConfig = mapMidiControllerConfig convertAudioInput convertChannel convertMidiKey config.controllers.midi
    audioConfig = fmap convertChannel (fromMaybe def config.audio)

    audioInputNames = getAudioInputNames config

    convertAudioInput = toAudioInputId . flip lookupNameRef audioInputNames

    channelNames = getChannelNames config

    convertChannel = toChannelId . flip lookupNameRef channelNames

    midiKeyNames = getMidiKeyNames config

    convertMidiKey = flip lookupNameRef midiKeyNames

getAudioInputNames :: Config -> NameMap
getAudioInputNames config =
  toNameMap $ mapMaybe getName $ zip (fromMaybe [] (fromMaybe def config.audio).inputs) [1 ..]
  where
    getName (input, n) = fmap (,n) (getInputName input)

    getInputName = \case
      StereoAudioInputConfig stereo -> stereo.name
      MonoAudioInputConfig mono -> mono.name

getChannelNames :: Config -> NameMap
getChannelNames config =
  toNameMap $ mapMaybe getName $ zip config.mixer.channels [1 ..]
  where
    getName (channel, n) = fmap (,n) channel.name

getMidiKeyNames :: Config -> NameMap
getMidiKeyNames config =
  NameMap $ fromMaybe mempty config.controllers.midi.keys

inlineConfig :: Config -> SE ()
inlineConfig config = do
  _ref <- newRef str
  pure ()
  where
    str :: Str
    str = fromString $ Text.unpack $ Text.decodeUtf8 $ Yaml.encode config
