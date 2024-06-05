module Live.Config.Types
  ( Config (..)
  , AudioConfig (..)
  ) where

import Live.Scene.Sampler.Config
import Live.Scene.Mixer.Fx.Config
import Live.Scene.Mixer.Config (MixerConfig (..))
import Live.Scene.Midi.Config (ControllerConfig (..))
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.TH qualified as Json

newtype AudioConfig = AudioConfig String
  deriving newtype (FromJSON, ToJSON)

data Config = Config
  { mixer :: MixerConfig
  , sampler :: SamplerConfig
  , audio :: AudioConfig
  , controllers :: ControllerConfig
  }

$(Json.deriveJSON Json.defaultOptions ''Config)


