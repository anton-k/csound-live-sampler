module Live.Config.Types (
  Config (..),
  ControllerConfig (..),
) where

import Data.Aeson.TH qualified as Json
import Live.Scene.AudioCard.Config
import Live.Scene.Common (NameRef)
import Live.Scene.Midi.Config (MidiControllerConfig (..))
import Live.Scene.Mixer.Config (MixerConfig (..))
import Live.Scene.Osc.Config (OscConfig (..))
import Live.Scene.Sampler.Config

data Config = Config
  { mixer :: MixerConfig NameRef
  , sampler :: SamplerConfig NameRef
  , audio :: Maybe (AudioConfig NameRef)
  , controllers :: ControllerConfig NameRef NameRef NameRef
  }

data ControllerConfig audioInput channel key = ControllerConfig
  { midi :: MidiControllerConfig audioInput channel key
  , osc :: Maybe OscConfig
  }

$(Json.deriveJSON Json.defaultOptions ''ControllerConfig)
$(Json.deriveJSON Json.defaultOptions ''Config)
