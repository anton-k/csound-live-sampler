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
  , FadersMidiConfig (..)
  , MutesMidiConfig (..)
  , ShiftMidiConfig (..)
  , TrackChangesMidiConfig (..)
  , readConfig
  ) where

import Data.Foldable (asum)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Yaml qualified as Yaml
import System.FilePath

data Config = Config
  { master :: MasterConfig
  , channels :: [ChannelConfig]
  , fxs :: [FxConfig] -- v2
  , tracks :: [TrackConfig]
  , audio :: AudioConfig
  , controllers :: ControllerConfig
  , dir :: Maybe FilePath
  }
  deriving (Generic, FromJSON, ToJSON)

data MasterConfig = MasterConfig
  { volume :: Float
  , gain :: Maybe Float
  }
  deriving (Generic, FromJSON, ToJSON)

data ChannelConfig = ChannelConfig
  { volume :: Float
  , gain :: Maybe Float
  }
  deriving (Generic, FromJSON, ToJSON)

data FxConfig = Reverb | Delay
  deriving (Generic, FromJSON, ToJSON)

data TrackConfig = TrackConfig
  { dir :: Maybe FilePath
  , name :: Text
  , stems :: [Stem]
  , cues :: [TimeSlot]
  , gain :: Maybe Float
  }
  deriving (Generic, FromJSON, ToJSON)

data Stem = Stem
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

data AudioConfig = AudioConfig String
  deriving (Generic, FromJSON, ToJSON)

data ControllerConfig = ControllerConfig
  { faders :: FadersMidiConfig
  , mutes :: MutesMidiConfig
  , shift :: ShiftMidiConfig
  , trackChanges :: TrackChangesMidiConfig
  }
  deriving (Generic, FromJSON, ToJSON)

data FadersMidiConfig = FadersMidiConfig
  { master :: Int
  , channels :: [Int]
  }
  deriving (Generic, FromJSON, ToJSON)

-- | Note numbers for the mutes
newtype MutesMidiConfig = MutesMidiConfig [Int]
  deriving newtype (FromJSON, ToJSON)

-- | Note numbers for the track changes
newtype TrackChangesMidiConfig = TrackChangesMidiConfig [Int]
  deriving newtype (FromJSON, ToJSON)

newtype ShiftMidiConfig = ShiftMidiConfig Int
  deriving newtype (FromJSON, ToJSON)

readConfig :: FilePath -> IO Config
readConfig file = appendAbsPath <$> do
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

-- | makes path absolute for stems
appendAbsPath :: Config -> Config
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
