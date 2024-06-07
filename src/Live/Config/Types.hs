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

data Config = Config
  { mixer :: MixerConfig
  , sampler :: SamplerConfig
  , audio :: AudioConfig
  , controllers :: ControllerConfig
  }

newtype AudioConfig = AudioConfig String
  deriving newtype (FromJSON, ToJSON)

data ControllerConfig = ControllerConfig
  { midi :: MidiControllerConfig
  }

$(Json.deriveJSON Json.defaultOptions ''ControllerConfig)
$(Json.deriveJSON Json.defaultOptions ''Config)


