module Live.Scene
  ( runScene
  ) where

import Live.Config
import Csound.Core
import Live.Scene.Midi
import Live.Scene.Mixer
import Live.Scene.Sampler

runScene :: Config -> IO ()
runScene config =
  dacBy (setMa <> setTrace) {- writeCsd "tmp.csd" -} $ do
  -- writeCsdBy (setMa <> setDac) "tmp.csd" $ do
    toAudio =<< loadScene config

data Scene = Scene
  { mixer :: Mixer
  , sampler :: Sampler
  }

-- * audio playback

toAudio :: Scene -> SE Sig2
toAudio scene =
  sum <$> (runGen (scene.sampler.audio) [])
--  sum <$> (runGen (scene.sampler.audio |> scene.mixer.audio) [])

-- * init

loadScene :: Config -> SE Scene
loadScene config = do
  mixer <- newMixer config.mixer
  sampler <- newSampler config.sampler
  setupMidi config mixer sampler
  pure $ Scene {..}
