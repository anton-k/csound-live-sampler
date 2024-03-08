module Live.Scene.Sampler.Config
  ( SamplerConfig (..)
  , TrackConfig (..)
  , StemConfig (..)
  , TimeSlot (..)
  , Cue (..)
  , appendAbsPath
  ) where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)
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
  , cues :: [TimeSlot]
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
  , cues :: [Cue]
  }
  deriving (Generic, FromJSON, ToJSON)

data Cue = Cue
  { start :: Float
  , dur :: Float
  }
  deriving (Generic, FromJSON, ToJSON)

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

