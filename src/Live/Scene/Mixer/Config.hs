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

data MixerConfig channelId = MixerConfig
  { channels :: [ChannelConfig channelId]
  , master :: Maybe MasterConfig
  }
  deriving (Functor)

data MasterConfig = MasterConfig
  { volume :: Float
  , gain :: Maybe Float
  , fxs :: Maybe FxChain
  }

instance Default MasterConfig where
  def = MasterConfig 1 Nothing Nothing

data ChannelConfig a = ChannelConfig
  { volume :: Float
  , gain :: Maybe Float
  , output :: Maybe a -- if Nothing then output to master
  , sends :: Maybe [SendConfig a]
  , fxs :: Maybe FxChain
  , name :: Maybe Text
  }
  deriving (Functor)

data SendConfig a = SendConfig
  { channel :: a
  , gain :: Float
  }
  deriving (Functor)

-- JSON instances

$(Json.deriveJSON Json.defaultOptions ''SendConfig)
$(Json.deriveJSON Json.defaultOptions ''ChannelConfig)
$(Json.deriveJSON Json.defaultOptions ''MasterConfig)
$(Json.deriveJSON Json.defaultOptions ''MixerConfig)
