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

import Data.Foldable (asum)
import Data.Text (Text)
import Data.Text qualified as Text
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
readConfig file = do
  config <- Yaml.decodeFileThrow file
  mErr <- validateConfig config
  case mErr of
    Nothing -> pure config
    Just err -> error $ Text.unpack err

-- | Nothing if everythong is ok
--
-- Checks that
--
-- * all files for stems exist
-- * volumes are within useful range
-- * audio and controllers are valid
-- * valid channels are used for stems
validateConfig :: Config -> IO (Maybe Text)
validateConfig config = do
  files <- checkFiles config
  vols <- checkVolumes config
  audio <- checkAudio config
  controls <- checkControllers config
  chans <- checkChannels config
  pure $ asum [files, vols, audio, controls, chans]
  where
    checkFiles _ = pure Nothing -- TODO
    checkVolumes _ = pure Nothing -- TODO
    checkAudio _ = pure Nothing -- TODO
    checkControllers _ = pure Nothing -- TODO
    checkChannels _ = pure Nothing -- TODO
