module Live.Scene.Sampler.Config
  ( SamplerConfig (..)
  , TrackConfig (..)
  , StemConfig (..)
  , TimeSlot (..)
  , Cue (..)
  , ClipsConfig (..)
  , ClipColumnConfig (..)
  , ClipName (..)
  , ColumnName (..)
  , ClipConfig (..)
  , ClipGroupConfig (..)
  , ClipMode (..)
  , NextAction (..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson qualified as Json
import GHC.Generics
import Data.Text (Text)
import Data.Text qualified as Text

data SamplerConfig = SamplerConfig
  { tracks :: [TrackConfig]
  , clips :: Maybe ClipsConfig
  , dir :: Maybe FilePath
  }
  deriving (Generic, FromJSON, ToJSON)

data TrackConfig = TrackConfig
  { dir :: Maybe FilePath
  , name :: Text
  , stems :: [StemConfig]
  , slots :: [TimeSlot]
  , gain :: Maybe Float
  }
  deriving (Generic, FromJSON, ToJSON)

data StemConfig = StemConfig
  { volume :: Maybe Float
  , file :: FilePath
  , channel :: Int
  , gain :: Maybe Float
  }
  deriving (Generic, FromJSON, ToJSON)

data TimeSlot = TimeSlot
  { bpm :: Float
  , measure :: Maybe (Int, Int)  -- default is 4/4
  , changeRate :: Maybe Int
  , cues :: [Cue]
  }
  deriving (Generic, FromJSON, ToJSON)

data Cue = Cue
  { start :: Maybe Int
  , dur :: Int
  , nextAction :: Maybe NextAction
  }
  deriving (Generic, FromJSON, ToJSON)

data NextAction = PlayLoop | PlayNext | StopPlayback
  deriving (Show, Eq, Enum)

data ClipsConfig = ClipsConfig
  { columns :: [ClipColumnConfig]
  , groups :: Maybe [ClipGroupConfig]
  , dir :: Maybe FilePath
  }
  deriving (Generic, FromJSON, ToJSON)

data ClipColumnConfig = ClipColumnConfig
  { name :: ColumnName
  , clips :: [ClipConfig]
  , dir :: Maybe FilePath
  , channel :: Maybe Int
  , gain :: Maybe Float
  }
  deriving (Generic, FromJSON, ToJSON)

newtype ColumnName = ColumnName
  { name :: Text
  }
  deriving newtype (FromJSON, ToJSON, Eq, Ord)

newtype ClipName = ClipName
  { name :: Text
  }
  deriving newtype (FromJSON, ToJSON, Eq, Ord)


data ClipGroupConfig = ClipGroupConfig
  { name :: Text
  , group :: [(ColumnName, ClipName)]
  }
  deriving (Generic, FromJSON, ToJSON)

data ClipConfig = ClipConfig
  { name :: ClipName
  , file :: FilePath
  , bpm :: Float
  , changeRate :: Maybe Int
  , start :: Maybe Int
  , dur :: Int
  , mode :: Maybe ClipMode
  , nextAction :: Maybe NextAction
  , channel :: Maybe Int
  , gain :: Maybe Float
  }
  deriving (Generic, FromJSON, ToJSON)

data ClipMode = Diskin | Loscil -- TODO: Mincer

instance ToJSON ClipMode where
  toJSON = \case
    Diskin -> "diskin"
    Loscil -> "loscil"

instance FromJSON ClipMode where
  parseJSON = Json.withText "ClipMode" $ \case
    "diskin" -> pure Diskin
    "loscil" -> pure Loscil
    other -> fail $ "Failed to parse: " <> Text.unpack other

instance ToJSON NextAction where
  toJSON = \case
    PlayLoop -> "loop"
    PlayNext -> "next"
    StopPlayback -> "stop"

instance FromJSON NextAction where
  parseJSON = Json.withText "NextAction" $ \case
    "loop" -> pure PlayLoop
    "next" -> pure PlayNext
    "stop" -> pure StopPlayback
    other -> fail $ Text.unpack ("Failed to parse: " <> other)

