module Live.Config.Types
  ( Config (..)
  , AudioConfig (..)
  ) where

import Live.Scene.Fx.Config
import Live.Scene.Sampler.Config
import Live.Scene.Mixer.Config (MixerConfig (..))
import Live.Scene.Midi.Config (ControllerConfig (..))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Config = Config
  { mixer :: MixerConfig
  , fxs :: [FxConfig]
  , sampler :: SamplerConfig
  , audio :: AudioConfig
  , controllers :: ControllerConfig
  }
  deriving (Generic, FromJSON, ToJSON)

data AudioConfig = AudioConfig String
  deriving (Generic, FromJSON, ToJSON)


