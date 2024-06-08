module Live.Scene
  ( runScene
  , writeSceneCsd
  ) where

import Live.Config
import Csound.Core
import Live.Scene.Midi
import Live.Scene.Mixer
import Live.Scene.Sampler
import Data.Maybe
import Live.Scene.Common (toChannelId)

writeSceneCsd :: Config -> Maybe FilePath -> IO ()
writeSceneCsd config mFile =
  writeCsdBy (setMa <> setDac) (fromMaybe "tmp.csd" mFile) $ do
    toAudio =<< loadScene config

-- | TODO: remove double rendering
runScene :: Config -> Maybe FilePath -> IO ()
runScene config mFile = do
  mapM_ (writeSceneCsd config . Just) mFile
  dacBy (setMa <> setTrace) $ do
    toAudio =<< loadScene config

data Scene = Scene
  { mixer :: Mixer
  , sampler :: Sampler
  }

-- * audio playback

toAudio :: Scene -> SE Sig2
toAudio scene = do
  scene.sampler.start
  scene.mixer.readMaster

-- * init

loadScene :: Config -> SE Scene
loadScene config = do
  channels <- newMixerChannels mixerConfig
  sampler <- newSampler samplerConfig (SamplerDeps $ appendMixerChannels channels)
  mixer <- newMixer mixerConfig channels sampler.readBpm
  setupMidi config mixer sampler
  pure $ Scene {..}
  where
    mixerConfig = fmap toChannelId config.mixer
    samplerConfig = fmap toChannelId config.sampler
