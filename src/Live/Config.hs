module Live.Config
  ( Config (..)
  , MixerConfig (..)
  , SamplerConfig (..)
  , ControllerConfig (..)
  , AudioConfig (..)
  , readConfig
  ) where

import Data.Text (Text)
import Data.Yaml qualified as Yaml
import Live.Scene.Mixer.Config (MixerConfig (..))
import Live.Scene.Sampler.Config
  (SamplerConfig (..), StemConfig (..), TrackConfig (..),
   ClipsConfig (..), ClipColumnConfig (..), ClipConfig (..),
  )
import System.FilePath
import Live.Scene.Sampler.Config qualified as SamplerConfig (SamplerConfig (..))
import Live.Scene.Sampler.Config qualified as StemConfig (StemConfig (..))
import Live.Scene.Sampler.Config qualified as ClipConfig (ClipConfig (..))
import Live.Scene.Sampler.Config qualified as ClipColumnConfig (ClipColumnConfig (..))
import Live.Config.Types
import Live.Config.Validate

readConfig :: FilePath -> IO (Either Text Config)
readConfig file = fmap absPath <$> do
  config <- Yaml.decodeFileThrow file
  checkConfig config
  where
    absPath config = config { sampler = appendAbsPath config.sampler }

appendAbsPath :: SamplerConfig a -> SamplerConfig a
appendAbsPath = appendStemAbsPath . appendClipAbsPath

-- | makes path absolute for stems
appendStemAbsPath :: SamplerConfig a -> SamplerConfig a
appendStemAbsPath config =
  config { tracks = fmap (appendTrack $ appendPath config.dir) config.tracks }
  where
    appendTrack addRoot track =
      track { stems = fmap (appendStem trackDir) track.stems }
      where
        trackDir = addRoot . appendPath track.dir

    appendStem :: (FilePath -> FilePath) -> StemConfig a -> StemConfig a
    appendStem add stem =
      stem { StemConfig.file = add stem.file }

appendClipAbsPath :: SamplerConfig a -> SamplerConfig a
appendClipAbsPath config =
  config { SamplerConfig.clips = fmap (go $ appendPath config.dir) config.clips }
  where
    go :: (FilePath -> FilePath) -> ClipsConfig a -> ClipsConfig a
    go add x =
      x { columns = fmap (appendColumn (add . appendPath x.dir)) x.columns }

    appendColumn :: (FilePath -> FilePath) -> ClipColumnConfig a -> ClipColumnConfig a
    appendColumn add column =
      column { ClipColumnConfig.clips = fmap (appendClip (add . appendPath column.dir)) column.clips }

    appendClip :: (FilePath -> FilePath) -> ClipConfig a -> ClipConfig a
    appendClip add clip =
      clip { ClipConfig.file = add clip.file }

appendPath :: Maybe FilePath -> FilePath -> FilePath
appendPath mPrefix = maybe id (</>)mPrefix
