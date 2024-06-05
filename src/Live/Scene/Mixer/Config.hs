module Live.Scene.Mixer.Config
  ( MixerConfig (..)
  , MasterConfig (..)
  , ChannelConfig (..)
  , SendConfig (..)
  ) where

import Data.Aeson.TH qualified as Json
import Live.Scene.Mixer.Fx.Config
import Data.Text (Text)

data MixerConfig = MixerConfig
  { channels :: [ChannelConfig]
  , master :: MasterConfig
  }

data MasterConfig = MasterConfig
  { volume :: Float
  , gain :: Maybe Float
  , fxs :: Maybe FxChain
  }

data ChannelConfig = ChannelConfig
  { volume :: Float
  , gain :: Maybe Float
  , output :: Maybe Int -- if Nothing then output to master
  , sends :: Maybe [SendConfig]
  , fxs :: Maybe FxChain
  , name :: Maybe Text
  }

data SendConfig = SendConfig
  { channel :: Int
  , gain :: Float
  }

-- JSON instances

$(Json.deriveJSON Json.defaultOptions ''SendConfig)
$(Json.deriveJSON Json.defaultOptions ''ChannelConfig)
$(Json.deriveJSON Json.defaultOptions ''MasterConfig)
$(Json.deriveJSON Json.defaultOptions ''MixerConfig)
