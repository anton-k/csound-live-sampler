module Live.Scene.Sampler.Config (
  SamplerConfig (..),
  TrackConfig (..),
  StemConfig (..),
  TimeSlot (..),
  Cue (..),
  ClipsConfig (..),
  ClipColumnConfig (..),
  ClipName (..),
  ColumnName (..),
  ClipConfig (..),
  ClipGroupConfig (..),
  ClipMode (..),
  NextAction (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Json
import Data.Aeson.TH qualified as Json
import Data.Text (Text)
import Data.Text qualified as Text

data SamplerConfig chan = SamplerConfig
  { tracks :: [TrackConfig chan]
  , clips :: Maybe (ClipsConfig chan)
  , dir :: Maybe FilePath
  , playlist :: Maybe [Text]
  }
  deriving (Functor)

data TrackConfig chan = TrackConfig
  { dir :: Maybe FilePath
  , name :: Text
  , stems :: [StemConfig chan]
  , slots :: [TimeSlot]
  , gain :: Maybe Float
  }
  deriving (Functor)

data StemConfig chan = StemConfig
  { volume :: Maybe Float
  , file :: FilePath
  , channel :: chan
  , gain :: Maybe Float
  }
  deriving (Functor)

data TimeSlot = TimeSlot
  { bpm :: Float
  , measure :: Maybe (Int, Int) -- default is 4/4
  , changeRate :: Maybe Int
  , cues :: [Cue]
  , timeScale :: Maybe Int
  }

data Cue = Cue
  { start :: Maybe Float
  , dur :: Float
  , nextAction :: Maybe NextAction
  }

data NextAction = PlayLoop | PlayNext | StopPlayback
  deriving (Show, Eq, Enum)

data ClipsConfig chan = ClipsConfig
  { columns :: [ClipColumnConfig chan]
  , groups :: Maybe [ClipGroupConfig]
  , dir :: Maybe FilePath
  }
  deriving (Functor)

data ClipColumnConfig a = ClipColumnConfig
  { name :: ColumnName
  , clips :: [ClipConfig a]
  , dir :: Maybe FilePath
  , channel :: Maybe a
  , gain :: Maybe Float
  }
  deriving (Functor)

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

data ClipConfig a = ClipConfig
  { name :: ClipName
  , file :: FilePath
  , bpm :: Float
  , measure :: Maybe (Int, Int)
  , changeRate :: Maybe Int
  , start :: Maybe Int
  , dur :: Int
  , mode :: Maybe ClipMode
  , nextAction :: Maybe NextAction
  , channel :: Maybe a
  , gain :: Maybe Float
  }
  deriving (Functor)

data ClipMode = Diskin | Flooper -- TODO: Mincer

-- JSON instances

instance ToJSON ClipMode where
  toJSON = \case
    Diskin -> "diskin"
    Flooper -> "flooper"

instance FromJSON ClipMode where
  parseJSON = Json.withText "ClipMode" $ \case
    "diskin" -> pure Diskin
    "flooper" -> pure Flooper
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

$(Json.deriveJSON Json.defaultOptions ''ClipConfig)
$(Json.deriveJSON Json.defaultOptions ''ClipGroupConfig)
$(Json.deriveJSON Json.defaultOptions ''ClipColumnConfig)
$(Json.deriveJSON Json.defaultOptions ''ClipsConfig)
$(Json.deriveJSON Json.defaultOptions ''Cue)
$(Json.deriveJSON Json.defaultOptions ''TimeSlot)
$(Json.deriveJSON Json.defaultOptions ''StemConfig)
$(Json.deriveJSON Json.defaultOptions ''TrackConfig)
$(Json.deriveJSON Json.defaultOptions ''SamplerConfig)
