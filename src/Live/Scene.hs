module Live.Scene
  ( runScene
  ) where

import Live.Config
import Csound.Core
import Live.Scene.Midi
import Live.Scene.Mixer
import Live.Scene.Sampler

import Live.Scene.Sampler.Engine

runEngine :: IO ()
runEngine =
  dacBy setTrace $ do
  -- writeCsdBy (setMa <> setDac) "tmp.csd" $ do
    instr1 <- newProc $ \(_size :: D) -> do
      writeOuts $ fromMono $ linseg [1, 0.5, 0] * osc 220

    instr2 <- newProc $ \(_size :: D) -> do
      writeOuts $ fromMono $ linseg [1, 0.5, 0] * osc 440

    engine <- newEngine

    instr3 <- newProc $ \(instrRef, bpm, size) ->
      engine.setPart $ Part
        { track = instrRef
        , clip = Clip
            { bpm = bpm
            , start = 0
            , changeRate = 16
            , beatSize = size
            , timeSize = 2
            }
        }

    -- play instr1 [Note 0 2 0]
    engine.start
    play instr3
      [ Note 1 1 (instr1, 120, 4)
      , Note 5 1 (instr2, 60, 4)
      , Note 20 1 (instr1, 120, 1)
      , Note 60 1 (instr2, 30, 4)
      ]


runScene :: Config -> IO ()
runScene config = runEngine
{-
  dacBy (setMa <> setTrace) {- writeCsd "tmp.csd" -} $ do
  -- writeCsdBy (setMa <> setDac) "tmp.csd" $ do
    toAudio =<< loadScene config
-}

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
