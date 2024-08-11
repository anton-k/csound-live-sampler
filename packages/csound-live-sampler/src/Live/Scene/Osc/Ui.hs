module Live.Scene.Osc.Ui (
  SceneUi (..),
  MixerUi (..),
  MixerChannelUi (..),
  FxUi (..),
  FxParamUi (..),
  SamplerUi (..),
  TrackUi (..),
  getUiConfig,
  getUiOscMessage,
) where

import Data.Aeson qualified as Json
import Data.Aeson.TH qualified as Json
import Data.ByteString qualified as ByteString
import Data.Default
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Live.Scene.Common (ChannelId (..))
import Live.Scene.Mixer.Config qualified as Config
import Live.Scene.Mixer.Fx (toFxName, toFxParamNameInitMap)
import Live.Scene.Mixer.Fx.Config qualified as Config
import Live.Scene.Mixer.Fx.Unit (FxParamName)
import Live.Scene.Sampler.Config qualified as Config

data SceneUi = SceneUi
  { mixer :: MixerUi
  , sampler :: SamplerUi
  }

-------------------------------------------------------------------------------------
-- Mixer

data MixerUi = MixerUi
  { channels :: [MixerChannelUi]
  , master :: MasterUi
  }

data MasterUi = MasterUi
  { volume :: Float
  , fxs :: [FxUi]
  }

data MixerChannelUi = MixerChannelUi
  { channel :: Int
  , volume :: Float
  , fxs :: [FxUi]
  , name :: Maybe Text
  }

data FxUi = FxUi
  { name :: Text
  , params :: [FxParamUi]
  }

data FxParamUi = FxParamUi
  { name :: Text
  , value :: Float
  }

-------------------------------------------------------------------------------------
-- Sampler

data SamplerUi = SamplerUi
  { tracks :: [TrackUi]
  }

data TrackUi = TrackUi
  { name :: Text
  , size :: Int
  }

-------------------------------------------------------------------------------------
-- JSON instances

$(Json.deriveJSON Json.defaultOptions ''TrackUi)
$(Json.deriveJSON Json.defaultOptions ''SamplerUi)
$(Json.deriveJSON Json.defaultOptions ''FxParamUi)
$(Json.deriveJSON Json.defaultOptions ''FxUi)
$(Json.deriveJSON Json.defaultOptions ''MasterUi)
$(Json.deriveJSON Json.defaultOptions ''MixerChannelUi)
$(Json.deriveJSON Json.defaultOptions ''MixerUi)
$(Json.deriveJSON Json.defaultOptions ''SceneUi)

-------------------------------------------------------------------------------------
-- OSC message

getUiOscMessage ::
  (IsString a) =>
  Config.MixerConfig ChannelId ->
  Config.SamplerConfig ChannelId ->
  a
getUiOscMessage mixer sampler =
  fromString $ Text.unpack $ Text.decodeUtf8 $ ByteString.toStrict $ Json.encode (getUiConfig mixer sampler)

getUiConfig ::
  Config.MixerConfig ChannelId ->
  Config.SamplerConfig ChannelId ->
  SceneUi
getUiConfig mixer sampler =
  SceneUi
    { mixer = getMixerUiConfig mixer
    , sampler = getSamplerUiConfig sampler
    }

getMixerUiConfig :: Config.MixerConfig ChannelId -> MixerUi
getMixerUiConfig config =
  MixerUi
    { channels = zipWith getChannelUiConfig [1 ..] config.channels
    , master = getMasterUiConfig (fromMaybe def config.master)
    }

getMasterUiConfig :: Config.MasterConfig -> MasterUi
getMasterUiConfig config =
  MasterUi
    { volume = config.volume
    , fxs = getFxUiConfig <$> fromMaybe [] config.fxs
    }

getChannelUiConfig :: Int -> Config.ChannelConfig ChannelId -> MixerChannelUi
getChannelUiConfig channelIndex config =
  MixerChannelUi
    { channel = channelIndex
    , volume = config.volume
    , fxs = getFxUiConfig <$> fromMaybe [] config.fxs
    , name = config.name
    }

getFxUiConfig :: Config.FxUnit -> FxUi
getFxUiConfig fx =
  FxUi
    { name = toFxName fx
    , params = fmap (uncurry getFxParamUi) $ Map.toList $ toFxParamNameInitMap fx
    }

getFxParamUi :: FxParamName -> Float -> FxParamUi
getFxParamUi name value = FxParamUi{name, value}

getSamplerUiConfig :: Config.SamplerConfig ChannelId -> SamplerUi
getSamplerUiConfig config =
  SamplerUi
    { tracks = fmap getTrackUiConfig config.tracks
    }

getTrackUiConfig :: Config.TrackConfig ChannelId -> TrackUi
getTrackUiConfig config =
  TrackUi
    { name = config.name
    , size = sum $ fmap (\slot -> length slot.cues) config.slots
    }
