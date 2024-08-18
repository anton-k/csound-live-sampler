module Scene.Mixer.Config
  ( MixerUi
  , MasterUiItem
  , MixerUiItem
  , Fx
  , FxUnit (..)
  , FxParam
  , SendFx
  , mixerUiFromJson
  , ChannelFxUi
  , toMixerFxUis
  , FxUi
  , toChannelName
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import JSON (JSON)
import JSON as Json
import JSON.Object as Json
import Data.Int (round)
import Data.Traversable (traverse)
import Common.JSON.Extra (lookupArray, lookupString, lookupNumber, lookupInt)
import Data.Array as Array
import Data.Maybe (fromMaybe)
import Action (ChannelId)
import Data.Tuple (Tuple (..), snd)
import Data.Tuple.Nested ((/\))

type MixerUi =
  { channels :: Array MixerUiItem
  , auxChannels :: Array MixerUiItem
  , master :: MasterUiItem
  }

type MasterUiItem =
  { volume :: Number
  , fxs :: Array Fx
  }

type MixerUiItem =
  { channel :: Int
  , volume :: Number
  , fxs :: Array Fx
  , name :: Maybe String
  , sends :: Array SendFx
  }

type Fx =
  { name :: String
  , unit :: FxUnit
  , params :: Array FxParam
  }

type FxParam =
  { name :: String
  , value :: Number
  }

type SendFx =
  { to :: Int
  , value :: Number
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

-- From JSON

mixerUiFromJson :: JSON -> Maybe MixerUi
mixerUiFromJson json = do
  obj <- Json.toJObject json
  channels <- traverse mixerUiItemFromJson =<< lookupArray "channels" obj
  auxChannels <- traverse mixerUiItemFromJson =<< lookupArray "auxChannels" obj
  master <- masterUiItemFromJson =<< Json.lookup "master" obj
  pure { channels, auxChannels, master }

masterUiItemFromJson :: JSON -> Maybe MasterUiItem
masterUiItemFromJson json = do
  obj <- Json.toJObject json
  volume <- lookupNumber "volume" obj
  fxs <- traverse fxFromJson =<< lookupArray "fxs" obj
  pure { volume, fxs }

mixerUiItemFromJson :: JSON -> Maybe MixerUiItem
mixerUiItemFromJson json = do
  obj <- Json.toJObject json
  channel <- lookupInt "channel" obj
  volume <- lookupNumber "volume" obj
  fxs <- traverse fxFromJson =<< lookupArray "fxs" obj
  sends <- traverse sendFxFromJson =<< lookupArray "sends" obj
  let
    name = lookupString "name" obj
  pure { channel, volume, fxs, sends, name }

fxFromJson :: JSON -> Maybe Fx
fxFromJson json = do
  obj <- Json.toJObject json
  name <- lookupString "name" obj
  unit <- fxUnitFromJson =<< lookupString "unit" obj
  params <- traverse fxParamFromJson =<< lookupArray "params" obj
  pure { name, unit, params }

fxUnitFromJson :: String -> Maybe FxUnit
fxUnitFromJson = case _ of
  "tool" -> Just ToolFxUnit
  "reverb" -> Just ReverbFxUnit
  "delay" -> Just DelayFxUnit
  "pingPong" -> Just PingPongFxUnit
  "moog" -> Just MoogFxUnit
  "korg" -> Just KorgFxUnit
  "bbcut" -> Just BbcutFxUnit
  "limiter" -> Just LimiterFxUnit
  "eq" -> Just EqFxUnit
  "mixerEq" -> Just MixerEqFxUnit
  _ -> Nothing

fxParamFromJson :: JSON -> Maybe FxParam
fxParamFromJson json = do
  obj <- Json.toJObject json
  name <- lookupString "name" obj
  value <- lookupNumber "value" obj
  pure { name, value }

sendFxFromJson :: JSON -> Maybe SendFx
sendFxFromJson json = do
  obj <- Json.toJObject json
  to <- lookupInt "to" obj
  value <- lookupNumber "value" obj
  pure { to, value }

type ChannelFxUi =
  { channelId :: Int
  , name :: String
  , fxs :: Array FxUi
  , sends :: Array SendFx
  }

type FxUi =
  { fx :: Fx
  , channel :: Maybe ChannelId
  }

toMixerFxUis :: MixerUi -> Array ChannelFxUi
toMixerFxUis ui =
  toMasterFxUis ui <> toChannelFxUis ui

toMasterFxUis :: MixerUi -> Array ChannelFxUi
toMasterFxUis ui =
  if Array.null fxs
    then []
    else pure $
      { channelId: 0
      , name: "master"
      , fxs: map
                (\fx -> { fx: fx, channel: Nothing })
                fxs
      , sends: []
      }
  where
    fxs = ui.master.fxs

toChannelFxUis :: MixerUi -> Array ChannelFxUi
toChannelFxUis ui =
  map (\(Tuple chanId chan) ->
          { channelId: fromMaybe 0 chanId
          , name: toChannelName chan
          , fxs: map (\fx -> { fx: fx, channel: chanId }) chan.fxs
          , sends: map succTo chan.sends
          }
      )
      fxChannels
  where
    fxChannels =
      Array.filter (isNonEmptyFx <<< snd) $
        Array.mapWithIndex (\n chan -> Just (n + 1) /\ chan) ui.channels

    succTo send =
      send { to = send.to + 1 }

toChannelName :: MixerUiItem -> String
toChannelName chan =
  fromMaybe ("Channel " <> show chan.channel) chan.name

isNonEmptyFx chan =
  Array.length chan.fxs /= 0 ||
  Array.length chan.sends /= 0
