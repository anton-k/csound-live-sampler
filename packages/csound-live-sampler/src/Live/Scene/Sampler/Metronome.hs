module Live.Scene.Sampler.Metronome (
  MetronomeInstr,
  newMetronomeInstr,
  playMetronome,
  MetronomeDeps (..),
) where

import Csound.Core
import Data.Boolean ((>*))
import Data.Maybe
import Live.Scene.Common (ChannelId (..))
import Live.Scene.Sampler.Config

type MetronomeInstr = InstrRef D

data MetronomeDeps = MetronomeDeps
  { writeChannel :: ChannelId -> Sig2 -> SE ()
  }

newMetronomeInstr :: MetronomeDeps -> MetronomeConfig ChannelId -> SE MetronomeInstr
newMetronomeInstr dep config =
  metronomeInstrBy writeOut tones
  where
    writeOut = dep.writeChannel config.channel . mul volumeGain

    tones = case fromMaybe NoiseMetronomeTone config.tone of
      NoiseMetronomeTone -> noiseTones
      StickMetronomeTone -> stickTones
      BeepMetronomeTone -> beepTones

metronomeInstrBy :: (Sig2 -> SE ()) -> MetronomeTones -> SE MetronomeInstr
metronomeInstrBy writeOut tones =
  newProc $ \isAccent -> do
    whens
      [(isAccent >* 0, writeOut =<< tones.accent)]
      (writeOut =<< tones.click)

data MetronomeTones = MetronomeTones
  { accent :: SE Sig2
  , click :: SE Sig2
  }

playMetronome :: MetronomeInstr -> BoolD -> SE ()
playMetronome instr isAccent =
  play instr [Note 0 0.25 (ifB isAccent 1 0)]

-- * metronome tones

noiseTones :: MetronomeTones
noiseTones =
  MetronomeTones
    { accent = noiseClickBy 0.9 5500
    , click = noiseClickBy 0.7 3000
    }

stickTones :: MetronomeTones
stickTones =
  MetronomeTones
    { accent = stickClickBy (400, 800) 0.9
    , click = stickClickBy (300, 600) 0.7
    }

beepTones :: MetronomeTones
beepTones =
  MetronomeTones
    { accent = beepClickBy 650 0.9
    , click = beepClickBy 600 0.75
    }

volumeGain :: Sig
volumeGain = 0.5

noiseClickBy :: Sig -> Sig -> SE Sig2
noiseClickBy freq vol =
  fmap fromMono $
    at (flip butlp (freq * linseg [1, 0.15, 0.5])) $
      mul (vol * linseg [0, 0.005, 1, 0.03, 1, 0.15, 0]) $
        white

stickClickBy :: (Sig, Sig) -> Sig -> SE Sig2
stickClickBy (minFreq, maxFreq) vol =
  pure $
    fromMono $
      mul vol $
        env * env * osc (minFreq + maxFreq * cpsEnv * cpsEnv)
  where
    env = expseg [1, 0.15, 0.0001]
    cpsEnv = expseg [1, 0.1, 0.0001]

beepClickBy :: Sig -> Sig -> SE Sig2
beepClickBy freq vol =
  pure $
    fromMono $
      vol * linseg [1, 0.075, 1, 0.075, 0] * osc freq
