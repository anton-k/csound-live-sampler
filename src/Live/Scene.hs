module Live.Scene
  ( runScene
  , writeSceneCsd
  ) where

import Live.Config
import Csound.Core
import Live.Scene.Midi
import Live.Scene.Mixer
import Live.Scene.Sampler
import Live.Scene.Audio
import Data.Maybe

writeSceneCsd :: Config -> Maybe FilePath -> IO ()
writeSceneCsd config mFile =
  writeCsdBy (withCsoundFlags config $ setMa <> setDac) (fromMaybe "tmp.csd" mFile) $ do
    toAudio =<< loadScene config

runScene :: Config -> Maybe FilePath -> IO ()
runScene config mFile = do
  dacBy (withOptions config mFile $ setMa <> setTrace) $ do
    toAudio =<< loadScene config

withOptions :: Config -> Maybe FilePath -> Options -> Options
withOptions config mFile = withWriteCsd mFile . withCsoundFlags config

data Scene = Scene
  { mixer :: Mixer
  , sampler :: Sampler
  , audio :: Audio
  }

-- * audio playback

toAudio :: Scene -> SE Sig2
toAudio scene = do
  scene.sampler.start
  result <- scene.mixer.readMaster
  scene.mixer.clean
  pure result

-- * init

-- | Note that order of definitions matters as we allocate
-- Csound instruments and instruments are executed in the order
-- of definition.
--
-- The order to define the instruments follows the flow of the signals:
--
-- * audio - reads audio inputs
-- * sampler - playback audio files
-- * mixer - mix signals to master outuput
loadScene :: Config -> SE Scene
loadScene config = do
  channels <- newMixerChannels mixerConfig
  let
    appendMixer = appendMixerChannels channels
  audio <- newAudio audioConfig (AudioDeps appendMixer)
  sampler <- newSampler samplerConfig (SamplerDeps appendMixer)
  mixer <- newMixer mixerConfig channels sampler.readBpm
  setupMidi audio mixer sampler midiConfig
  pure $ Scene {audio, sampler, mixer}
  where
    (audioConfig, samplerConfig, mixerConfig, midiConfig) = convertConfig config

withCsoundFlags :: Config -> Options -> Options
withCsoundFlags config = maybe id (<>) $ do
  audio <- config.audio
  flags <- audio.csound
  pure $ setVerbatimFlags flags

withWriteCsd :: Maybe FilePath -> Options -> Options
withWriteCsd mFile = maybe id (mappend . setWriteCsd) mFile

