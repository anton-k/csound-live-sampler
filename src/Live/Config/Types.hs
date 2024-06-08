module Live.Config.Types
  ( Config (..)
  , AudioConfig (..)
  , ControllerConfig (..)
  ) where

import Live.Scene.Sampler.Config
import Live.Scene.Mixer.Config (MixerConfig (..))
import Live.Scene.Midi.Config (MidiControllerConfig (..))
import Data.Aeson.TH qualified as Json
import Live.Scene.Common (NameRef)
import Data.Text (Text)

data Config = Config
  { mixer :: MixerConfig NameRef
  , sampler :: SamplerConfig NameRef
  , audio :: Maybe AudioConfig
  , controllers :: ControllerConfig NameRef
  }

data AudioConfig = AudioConfig
  { csound :: Maybe Text
  }

data ControllerConfig channel = ControllerConfig
  { midi :: MidiControllerConfig channel
  }

$(Json.deriveJSON Json.defaultOptions ''AudioConfig)
$(Json.deriveJSON Json.defaultOptions ''ControllerConfig)
$(Json.deriveJSON Json.defaultOptions ''Config)


