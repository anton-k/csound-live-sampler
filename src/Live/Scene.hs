module Live.Scene
  ( runScene
  , writeSceneCsd
  ) where

import Data.Bifunctor
import Live.Config
import Csound.Core
import Live.Scene.Midi
import Live.Scene.Midi.Config
import Live.Scene.Mixer
import Live.Scene.Sampler
import Data.Maybe
import Live.Scene.Common

writeSceneCsd :: Config -> Maybe FilePath -> IO ()
writeSceneCsd config mFile =
  writeCsdBy (withCsoundFlags config $ setMa <> setDac) (fromMaybe "tmp.csd" mFile) $ do
    toAudio =<< loadScene config

-- | TODO: remove double rendering
runScene :: Config -> Maybe FilePath -> IO ()
runScene config mFile = do
  mapM_ (writeSceneCsd config . Just) mFile
  dacBy (withCsoundFlags config $ setMa <> setTrace) $ do
    toAudio =<< loadScene config

data Scene = Scene
  { mixer :: Mixer
  , sampler :: Sampler
  }

-- * audio playback

toAudio :: Scene -> SE Sig2
toAudio scene = do
  scene.sampler.start
  result <- scene.mixer.readMaster
  scene.mixer.clean
  pure result

-- * init

loadScene :: Config -> SE Scene
loadScene config = do
  channels <- newMixerChannels mixerConfig
  sampler <- newSampler samplerConfig (SamplerDeps $ appendMixerChannels channels)
  mixer <- newMixer mixerConfig channels sampler.readBpm
  setupMidi mixer sampler midiConfig
  pure $ Scene {..}
  where
    mixerConfig = fmap convertChannel config.mixer
    samplerConfig = fmap convertChannel config.sampler
    midiConfig = bimap convertChannel convertMidiKey config.controllers.midi

    channelNames = getChannelNames config

    convertChannel = toChannelId . flip lookupNameRef channelNames

    midiKeyNames = getMidiKeyNames config

    convertMidiKey = flip lookupNameRef midiKeyNames

getChannelNames :: Config -> NameMap
getChannelNames config =
  toNameMap $ mapMaybe getName $ zip config.mixer.channels [1..]
    where
      getName (channel, n) = fmap (, n) channel.name

getMidiKeyNames :: Config -> NameMap
getMidiKeyNames config =
  NameMap $ fromMaybe mempty config.controllers.midi.keys

withCsoundFlags :: Config -> Options -> Options
withCsoundFlags config = maybe id (<>) $ do
  audio <- config.audio
  flags <- audio.csound
  pure $ setVerbatimFlags flags
