module Live.Scene (
  runScene,
  writeSceneCsd,
) where

import Csound.Core hiding (Config)
import Data.Maybe
import Live.Config
import Live.Scene.AudioCard
import Live.Scene.Midi
import Live.Scene.Mixer
import Live.Scene.Osc
import Live.Scene.Sampler
import Live.Scene.Types

writeSceneCsd :: Config -> Maybe FilePath -> IO ()
writeSceneCsd config mFile =
  writeCsdBy (withCsoundFlags config $ setMa <> setDac) (fromMaybe "tmp.csd" mFile) $ do
    execScene =<< loadScene config

runScene :: Config -> Maybe FilePath -> IO ()
runScene config mFile = do
  dacBy (withOptions config mFile $ setMa <> setTrace) $ do
    execScene =<< loadScene config

withOptions :: Config -> Maybe FilePath -> Options -> Options
withOptions config mFile = withWriteCsd mFile . withCsoundFlags config

-- * audio playback

{-
toAudio :: Scene -> SE Sig2
toAudio scene = do
  scene.sampler.start
  result <- scene.mixer.readMaster
  scene.mixer.clean
  pure result
-}

execScene :: Scene -> SE ()
execScene scene = do
  scene.sampler.start
  scene.audio.setupOutputs
  scene.mixer.clean

-- * init

{-| Note that order of definitions matters as we allocate
Csound instruments and instruments are executed in the order
of definition.

The order to define the instruments follows the flow of the signals:

* audio - reads audio inputs
* sampler - playback audio files
* mixer - mix signals to master outuput
-}
loadScene :: Config -> SE Scene
loadScene config = do
  channels <- newMixerChannels mixerConfig
  let
    appendMixer = appendMixerChannels channels
    readMixer = readMixerChannels channels
    readMaster = readMixerMaster channels
  audio <- newAudioCard audioConfig (AudioCardDeps appendMixer readMixer readMaster)
  sampler <- newSampler samplerConfig (SamplerDeps appendMixer)
  mixer <- newMixer mixerConfig channels sampler.readBpm
  let
    scene = Scene{audio, sampler, mixer}
  setupMidi audio mixer sampler midiConfig
  mapM_ (\oscConfig -> setupOsc (OscConfigs oscConfig mixerConfig audioConfig) scene) mOscConfig
  pure scene
  where
    (audioConfig, samplerConfig, mixerConfig, midiConfig) = convertConfig config
    mOscConfig = config.controllers.osc

withCsoundFlags :: Config -> Options -> Options
withCsoundFlags config = maybe id (<>) $ do
  audio <- config.audio
  flags <- audio.csound
  pure $ setVerbatimFlags flags

withWriteCsd :: Maybe FilePath -> Options -> Options
withWriteCsd mFile = maybe id (mappend . setWriteCsd) mFile
