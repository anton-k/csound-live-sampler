module Live.Config.Types
  ( Config (..)
  , AudioConfig (..)
  , ControllerConfig (..)
  ) where

import Live.Scene.Sampler.Config
import Live.Scene.Mixer.Config (MixerConfig (..))
import Live.Scene.Midi.Config (MidiControllerConfig (..))
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.TH qualified as Json
import Live.Scene.Common (NameRef)

data Config = Config
  { mixer :: MixerConfig NameRef
  , sampler :: SamplerConfig NameRef
  , audio :: AudioConfig
  , controllers :: ControllerConfig NameRef
  }

newtype AudioConfig = AudioConfig String
  deriving newtype (FromJSON, ToJSON)

data ControllerConfig channel = ControllerConfig
  { midi :: MidiControllerConfig channel
  }

$(Json.deriveJSON Json.defaultOptions ''ControllerConfig)
$(Json.deriveJSON Json.defaultOptions ''Config)


