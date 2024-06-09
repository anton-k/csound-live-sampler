module Live.Config.Types
  ( Config (..)
  , ControllerConfig (..)
  ) where

import Live.Scene.Audio.Config
import Live.Scene.Sampler.Config
import Live.Scene.Mixer.Config (MixerConfig (..))
import Live.Scene.Midi.Config (MidiControllerConfig (..))
import Data.Aeson.TH qualified as Json
import Live.Scene.Common (NameRef)

data Config = Config
  { mixer :: MixerConfig NameRef
  , sampler :: SamplerConfig NameRef
  , audio :: Maybe (AudioConfig NameRef)
  , controllers :: ControllerConfig NameRef NameRef NameRef
  }

data ControllerConfig audioInput channel key = ControllerConfig
  { midi :: MidiControllerConfig audioInput channel key
  }

$(Json.deriveJSON Json.defaultOptions ''ControllerConfig)
$(Json.deriveJSON Json.defaultOptions ''Config)
