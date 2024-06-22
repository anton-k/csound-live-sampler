module Live.Scene.Osc.Config (
  OscConfig (..),
  OscInputConfig (..),
  OscOutputConfig (..),
) where

import Data.Aeson.TH qualified as Json

data OscConfig = OscConfig
  { input :: Maybe OscInputConfig
  , output :: Maybe OscOutputConfig
  }

data OscInputConfig = OscInputConfig
  { port :: Int
  }

data OscOutputConfig = OscOutputConfig
  { port :: Int
  }

$(Json.deriveJSON Json.defaultOptions ''OscInputConfig)
$(Json.deriveJSON Json.defaultOptions ''OscOutputConfig)
$(Json.deriveJSON Json.defaultOptions ''OscConfig)
