module Live.Scene.Mixer.Config
  ( MixerConfig (..)
  , MasterConfig (..)
  , ChannelConfig (..)
  ) where

import Data.Aeson.TH qualified as Json

data MixerConfig = MixerConfig
  { channels :: [ChannelConfig]
  , master :: MasterConfig
  }

data MasterConfig = MasterConfig
  { volume :: Float
  , gain :: Maybe Float
  }

data ChannelConfig = ChannelConfig
  { volume :: Float
  , gain :: Maybe Float
  }

-- JSON instances

$(Json.deriveJSON Json.defaultOptions ''ChannelConfig)
$(Json.deriveJSON Json.defaultOptions ''MasterConfig)
$(Json.deriveJSON Json.defaultOptions ''MixerConfig)
