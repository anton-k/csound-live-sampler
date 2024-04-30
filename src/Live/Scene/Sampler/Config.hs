module Live.Scene.Sampler.Config
  ( SamplerConfig (..)
  , TrackConfig (..)
  , StemConfig (..)
  , TimeSlot (..)
  , Cue (..)
  , NextAction (..)
  , appendAbsPath
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson qualified as Json
import GHC.Generics
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath

data SamplerConfig = SamplerConfig
  { tracks :: [TrackConfig]
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

-- | makes path absolute for stems
appendAbsPath :: SamplerConfig -> SamplerConfig
appendAbsPath config =
  config { tracks = fmap (appendTrack config.dir) config.tracks }
  where
    appendTrack mRootDir track =
      track { stems = fmap (appendStem trackDir) track.stems }
      where
        trackDir =
          case mRootDir of
            Nothing -> track.dir
            Just rootDir ->
              Just $ case track.dir of
                Nothing -> rootDir
                Just dir -> rootDir </> dir

    appendStem mTrackDir stem =
      stem { file = maybe id (</>) mTrackDir stem.file }

