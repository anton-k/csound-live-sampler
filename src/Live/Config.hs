module Live.Config
  ( Config (..)
  , MixerConfig (..)
  , SamplerConfig (..)
  , ControllerConfig (..)
  , FxConfig (..)
  , AudioConfig (..)
  , readConfig
  ) where

import Data.Foldable (asum)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Yaml qualified as Yaml
import Live.Scene.Mixer.Config (MixerConfig (..))
import Live.Scene.Midi.Config (ControllerConfig (..))
import Live.Scene.Sampler.Config (SamplerConfig (..), appendAbsPath)

data Config = Config
  { mixer :: MixerConfig
  , fxs :: [FxConfig] -- v2
  , sampler :: SamplerConfig
  , audio :: AudioConfig
  , controllers :: ControllerConfig
  }
  deriving (Generic, FromJSON, ToJSON)

data FxConfig = Reverb | Delay
  deriving (Generic, FromJSON, ToJSON)

data AudioConfig = AudioConfig String
  deriving (Generic, FromJSON, ToJSON)

readConfig :: FilePath -> IO Config
readConfig file = absPath <$> do
  config <- Yaml.decodeFileThrow file
  mErr <- validateConfig config
  case mErr of
    Nothing -> pure config
    Just err -> error $ Text.unpack err
  where
    absPath config = config { sampler = appendAbsPath config.sampler }

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
