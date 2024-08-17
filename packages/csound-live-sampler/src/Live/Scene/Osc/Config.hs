module Live.Scene.Osc.Config (
  OscConfig (..),
  OscInputConfig (..),
  OscOutputConfig (..),
  OscUiConfig (..),
  ShowControls (..),
) where

import Data.Aeson.TH qualified as Json
import Data.Default
import Data.Text (Text)

data OscConfig channel = OscConfig
  { input :: Maybe OscInputConfig
  , output :: Maybe (OscOutputConfig channel)
  , ui :: Maybe (OscUiConfig channel)
  }
  deriving (Functor)

data OscUiConfig channel = OscUiConfig
  { mainChannels :: Maybe [channel]
  , auxChannels :: Maybe [channel]
  , showControls :: Maybe ShowControls
  }
  deriving (Functor)

instance Default (OscUiConfig channel) where
  def = OscUiConfig def def def

data ShowControls = ShowControls
  { auxChannels :: Bool
  , fxs :: Bool
  , audio :: Bool
  , mutes :: Bool
  }

instance Default ShowControls where
  def = ShowControls False False False False

data OscInputConfig = OscInputConfig
  { port :: Int
  }

data OscOutputConfig channel = OscOutputConfig
  { address :: Text
  , port :: Int
  , channels :: Maybe [channel]
  }
  deriving (Functor)

$(Json.deriveJSON Json.defaultOptions ''ShowControls)
$(Json.deriveJSON Json.defaultOptions ''OscUiConfig)
$(Json.deriveJSON Json.defaultOptions ''OscInputConfig)
$(Json.deriveJSON Json.defaultOptions ''OscOutputConfig)
$(Json.deriveJSON Json.defaultOptions ''OscConfig)
