module Live.Scene.Mixer.Config
  ( MixerConfig (..)
  , MasterConfig (..)
  , ChannelConfig (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data MixerConfig = MixerConfig
  { channels :: [ChannelConfig]
  , master :: MasterConfig
  }
  deriving (Generic, FromJSON, ToJSON)

data MasterConfig = MasterConfig
  { volume :: Float
  , gain :: Maybe Float
  }
  deriving (Generic, FromJSON, ToJSON)

data ChannelConfig = ChannelConfig
  { volume :: Float
  , gain :: Maybe Float
  }
  deriving (Generic, FromJSON, ToJSON)
