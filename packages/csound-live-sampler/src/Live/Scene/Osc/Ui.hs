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
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Live.Scene.AudioCard qualified as AudioCard
import Live.Scene.AudioCard.Config qualified as Config
import Live.Scene.Common (ChannelId (..))
import Live.Scene.Mixer.Config qualified as Config
import Live.Scene.Mixer.Fx (toFxName, toFxParamNameInitMap)
import Live.Scene.Mixer.Fx.Config qualified as Config
import Live.Scene.Mixer.Fx.Unit (FxParamName)
import Live.Scene.Osc.Config qualified as Config
import Live.Scene.Sampler (orderTracks)
import Live.Scene.Sampler.Config qualified as Config

data SceneUi = SceneUi
  { mixer :: MixerUi
  , sampler :: SamplerUi
  , audioCard :: AudioCardUi
  }

-------------------------------------------------------------------------------------
-- Mixer

data MixerUi = MixerUi
  { channels :: [MixerChannelUi]
  -- ^ main channels are shown on the big pannel in the UI
  , auxChannels :: [MixerChannelUi]
  -- ^ aux channels are shown on the accordion-pad in the UI (small version)
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
  , sends :: [SendFxUi]
  , name :: Maybe Text
  }

data FxUi = FxUi
  { name :: Text
  , unit :: FxUnit
  , params :: [FxParamUi]
  }

data FxParamUi = FxParamUi
  { name :: Text
  , value :: Float
  }

data SendFxUi = SendFxUi
  { to :: Int
  , value :: Float
  }

data FxUnit
  = ToolFxUnit
  | ReverbFxUnit
  | DelayFxUnit
  | PingPongFxUnit
  | MoogFxUnit
  | KorgFxUnit
  | BbcutFxUnit
  | LimiterFxUnit
  | EqFxUnit
  | MixerEqFxUnit

data AudioCardUi = AudioCardUi
  { inputs :: [Int]
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

instance Json.ToJSON FxUnit where
  toJSON = \case
    ToolFxUnit -> "tool"
    ReverbFxUnit -> "reverb"
    DelayFxUnit -> "delay"
    PingPongFxUnit -> "pingPong"
    MoogFxUnit -> "moog"
    KorgFxUnit -> "korg"
    BbcutFxUnit -> "bbcut"
    LimiterFxUnit -> "limiter"
    EqFxUnit -> "eq"
    MixerEqFxUnit -> "mixerEq"

instance Json.FromJSON FxUnit where
  parseJSON = Json.withText "FxUnit" $ \case
    "tool" -> pure ToolFxUnit
    "reverb" -> pure ReverbFxUnit
    "delay" -> pure DelayFxUnit
    "pingPong" -> pure PingPongFxUnit
    "moog" -> pure MoogFxUnit
    "korg" -> pure KorgFxUnit
    "bbcut" -> pure BbcutFxUnit
    "limiter" -> pure LimiterFxUnit
    "eq" -> pure EqFxUnit
    "mixerEq" -> pure MixerEqFxUnit
    other -> fail $ "failed to parse as fx unit: " <> Text.unpack other

$(Json.deriveJSON Json.defaultOptions ''AudioCardUi)
$(Json.deriveJSON Json.defaultOptions ''TrackUi)
$(Json.deriveJSON Json.defaultOptions ''SamplerUi)
$(Json.deriveJSON Json.defaultOptions ''FxParamUi)
$(Json.deriveJSON Json.defaultOptions ''FxUi)
$(Json.deriveJSON Json.defaultOptions ''SendFxUi)
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
  Config.OscUiConfig ChannelId ->
  Config.AudioConfig ChannelId ->
  a
getUiOscMessage mixer sampler ui audioCard =
  fromString $ Text.unpack $ Text.decodeUtf8 $ ByteString.toStrict $ Json.encode (getUiConfig mixer sampler ui audioCard)

getUiConfig ::
  Config.MixerConfig ChannelId ->
  Config.SamplerConfig ChannelId ->
  Config.OscUiConfig ChannelId ->
  Config.AudioConfig ChannelId ->
  SceneUi
getUiConfig mixer sampler ui audioCard =
  SceneUi
    { mixer = getMixerUiConfig mixer ui
    , sampler = getSamplerUiConfig sampler
    , audioCard = getAudioCardUiConfig audioCard
    }

getAudioCardUiConfig :: Config.AudioConfig ChannelId -> AudioCardUi
getAudioCardUiConfig config =
  AudioCardUi
    { inputs = maybe [] (fmap getInput) config.inputs
    }
  where
    getInput :: Config.AudioInputConfig ChannelId -> Int
    getInput = unChannelId . AudioCard.getChannelIdConfig

getMixerUiConfig :: Config.MixerConfig ChannelId -> Config.OscUiConfig ChannelId -> MixerUi
getMixerUiConfig mixer ui =
  MixerUi{channels, auxChannels, master}
  where
    master = getMasterUiConfig (fromMaybe def mixer.master)

    (channels, auxChannels) = getChannels

    getChannels =
      case ui.mainChannels of
        Just uiChannels ->
          case ui.auxChannels of
            Just auxChannels -> channelsAuxs uiChannels auxChannels
            Nothing -> channelsNoAuxs uiChannels
        Nothing ->
          case ui.auxChannels of
            Just auxChannels -> noChannelsAuxs auxChannels
            Nothing -> noChannelsNoAuxs
      where
        allChannels :: Map ChannelId MixerChannelUi
        allChannels =
          Map.fromList
            ( zipWith
                (\chanId conf -> (ChannelId chanId, getChannelUiConfig chanId conf))
                [0 ..]
                mixer.channels
            )

        allKeys = Map.keysSet allChannels

        channelsAuxs uiChannels uiAuxs = toResult uiChannels uiAuxs

        channelsNoAuxs uiChannels = toResult uiChannels (invert uiChannels)

        noChannelsAuxs uiAuxs = toResult (invert uiAuxs) uiAuxs

        noChannelsNoAuxs = toResult uiChannels uiAuxs
          where
            (uiChannels, uiAuxs) = List.splitAt 10 (Set.toList allKeys)

        toResult mainIds auxIds = (select mainIds, select auxIds)

        select :: [ChannelId] -> [MixerChannelUi]
        select ids =
          Map.elems $
            Map.restrictKeys allChannels (Set.fromList ids)

        invert :: [ChannelId] -> [ChannelId]
        invert ids =
          Set.toList (Set.difference allKeys (Set.fromList ids))

getMasterUiConfig :: Config.MasterConfig -> MasterUi
getMasterUiConfig config =
  MasterUi
    { volume = config.volume
    , fxs = getFxUiConfig <$> fromMaybe [] config.fxs
    }

getChannelUiConfig :: Int -> Config.ChannelConfig ChannelId -> MixerChannelUi
getChannelUiConfig channelIndex config =
  MixerChannelUi
    { channel = channelIndex + 1 -- turns to human-readable 1-based index
    , volume = config.volume
    , fxs = getFxUiConfig <$> fromMaybe [] config.fxs
    , sends = getSendFxUiConfig <$> fromMaybe [] config.sends
    , name = config.name
    }

getFxUiConfig :: Config.FxUnit -> FxUi
getFxUiConfig fx =
  FxUi
    { name = toFxName fx
    , unit = toFxUnit fx
    , params = fmap (uncurry getFxParamUi) $ Map.toList $ toFxParamNameInitMap fx
    }

toFxUnit :: Config.FxUnit -> FxUnit
toFxUnit = \case
  Config.ToolFx _ -> ToolFxUnit
  Config.ReverbFx _ -> ReverbFxUnit
  Config.DelayFx _ -> DelayFxUnit
  Config.PingPongFx _ -> PingPongFxUnit
  Config.MoogFx _ -> MoogFxUnit
  Config.KorgFx _ -> KorgFxUnit
  Config.BbcutFx _ -> BbcutFxUnit
  Config.LimiterFx _ -> LimiterFxUnit
  Config.EqFx _ -> EqFxUnit
  Config.MixerEqFx _ -> MixerEqFxUnit

getFxParamUi :: FxParamName -> Float -> FxParamUi
getFxParamUi name value = FxParamUi{name, value}

getSendFxUiConfig :: Config.SendConfig ChannelId -> SendFxUi
getSendFxUiConfig config =
  SendFxUi
    { to = config.channel.unChannelId
    , value = config.gain
    }

getSamplerUiConfig :: Config.SamplerConfig ChannelId -> SamplerUi
getSamplerUiConfig config =
  SamplerUi
    { tracks = fmap getTrackUiConfig (orderTracks config).tracks
    }

getTrackUiConfig :: Config.TrackConfig ChannelId -> TrackUi
getTrackUiConfig config =
  TrackUi
    { name = config.name
    , size = sum $ fmap (\slot -> length slot.cues) config.slots
    }
