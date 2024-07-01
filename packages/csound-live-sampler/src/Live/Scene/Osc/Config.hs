module Live.Scene.Osc.Config (
  OscConfig (..),
  OscInputConfig (..),
  OscOutputConfig (..),
) where

import Data.Aeson.TH qualified as Json
import Data.Text (Text)

data OscConfig channel = OscConfig
  { input :: Maybe OscInputConfig
  , output :: Maybe (OscOutputConfig channel)
  }
  deriving (Functor)

data OscInputConfig = OscInputConfig
  { port :: Int
  }

data OscOutputConfig channel = OscOutputConfig
  { address :: Text
  , port :: Int
  , channels :: Maybe [channel]
  }
  deriving (Functor)

$(Json.deriveJSON Json.defaultOptions ''OscInputConfig)
$(Json.deriveJSON Json.defaultOptions ''OscOutputConfig)
$(Json.deriveJSON Json.defaultOptions ''OscConfig)
