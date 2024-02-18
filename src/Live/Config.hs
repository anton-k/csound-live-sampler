module Live.Config
  ( Config (..)
  , MasterConfig (..)
  , ChannelConfig (..)
  , FxConfig (..)
  , TrackConfig (..)
  , Stem (..)
  , TimeSlot (..)
  , Cue (..)
  , AudioConfig (..)
  , ControllerConfig (..)
  , readConfig
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Yaml qualified as Yaml

data Config = Config
  { master :: MasterConfig
  , channels :: [ChannelConfig]
  , fxs :: [FxConfig] -- v2
  , tracks :: [TrackConfig]
  , audio :: AudioConfig
  , controllers :: ControllerConfig
  }
  deriving (Generic, FromJSON, ToJSON)

data MasterConfig = MasterConfig
  { volume :: Float
  }
  deriving (Generic, FromJSON, ToJSON)

data ChannelConfig = ChannelConfig
  { volume :: Float
  }
  deriving (Generic, FromJSON, ToJSON)

data FxConfig = Reverb | Delay
  deriving (Generic, FromJSON, ToJSON)

data TrackConfig = TrackConfig
  { name :: Text
  , stems :: [Stem]
  , cues :: [TimeSlot]
  }
  deriving (Generic, FromJSON, ToJSON)

data Stem = Stem
  { volume :: Float
  , file :: FilePath
  , channel :: Int
  }
  deriving (Generic, FromJSON, ToJSON)

data TimeSlot = TimeSlot
  { bpm :: Float
  , measure :: Maybe (Int, Int)  -- default is 4/4
  , cues :: [Cue]
  }
  deriving (Generic, FromJSON, ToJSON)

data Cue = Cue
  { start :: Float
  , dur :: Float
  }
  deriving (Generic, FromJSON, ToJSON)

data AudioConfig = AudioConfig String
  deriving (Generic, FromJSON, ToJSON)

data ControllerConfig = ControllerConfig String
  deriving (Generic, FromJSON, ToJSON)

readConfig :: FilePath -> IO Config
readConfig = Yaml.decodeFileThrow
