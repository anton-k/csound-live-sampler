module Live.Scene.Mixer.Config
  ( MixerConfig (..)
  , MasterConfig (..)
  , ChannelConfig (..)
  , SendConfig (..)
  ) where

import Data.Aeson.TH qualified as Json
import Live.Scene.Mixer.Fx.Config
import Data.Text (Text)
import Data.Default

data MixerConfig = MixerConfig
  { channels :: [ChannelConfig]
  , master :: Maybe MasterConfig
  }

data MasterConfig = MasterConfig
  { volume :: Float
  , gain :: Maybe Float
  , fxs :: Maybe FxChain
  }

instance Default MasterConfig where
  def = MasterConfig 1 Nothing Nothing

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
