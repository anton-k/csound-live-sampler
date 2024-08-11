module Scene.Mixer.Config
  ( MixerUi
  , MasterUiItem
  , MixerUiItem
  , Fx
  , FxParam
  , mixerUiFromJson
  , ChannelFxUi
  , toMixerFxUis
  , FxUi
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import JSON (JSON)
import JSON as Json
import JSON.Object as Json
import Data.Int (round)
import Data.Traversable (traverse)
import JSON.Extra (lookupArray, lookupString, lookupNumber, lookupInt)
import Data.Array as Array
import Data.Maybe (fromMaybe)
import Action (ChannelId)
import Data.Tuple (Tuple (..), snd)
import Data.Tuple.Nested ((/\))

type MixerUi =
  { channels :: Array MixerUiItem
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
  }

type Fx =
  { name :: String
  , params :: Array FxParam
  }

type FxParam =
  { name :: String
  , value :: Number
  }

-- From JSON

mixerUiFromJson :: JSON -> Maybe MixerUi
mixerUiFromJson json = do
  obj <- Json.toJObject json
  channels <- traverse mixerUiItemFromJson =<< lookupArray "channels" obj
  master <- masterUiItemFromJson =<< Json.lookup "master" obj
  pure { channels, master }

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
  let
    name = lookupString "name" obj
  pure { channel, volume, fxs, name }

fxFromJson :: JSON -> Maybe Fx
fxFromJson json = do
  obj <- Json.toJObject json
  name <- lookupString "name" obj
  params <- traverse fxParamFromJson =<< lookupArray "params" obj
  pure { name, params }

fxParamFromJson :: JSON -> Maybe FxParam
fxParamFromJson json = do
  obj <- Json.toJObject json
  name <- lookupString "name" obj
  value <- lookupNumber "value" obj
  pure { name, value }

type ChannelFxUi =
  { name :: String
  , fxs :: Array FxUi
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
      { name: "master"
      , fxs: map
                (\fx -> { fx: fx, channel: Nothing })
                fxs
      }
  where
    fxs = ui.master.fxs

toChannelFxUis :: MixerUi -> Array ChannelFxUi
toChannelFxUis ui =
  map (\(Tuple chanId chan) ->
          { name: toChannelName chan
          , fxs: map (\fx -> { fx: fx, channel: chanId }) chan.fxs
          }
      )
      fxChannels
  where
    fxChannels =
      Array.filter (isNonEmptyFx <<< (_.fxs) <<< snd) $
        Array.mapWithIndex (\n chan -> Just (n + 1) /\ chan) ui.channels

    toChannelName :: MixerUiItem -> String
    toChannelName chan =
      fromMaybe ("Channel " <> show chan.channel) chan.name

isNonEmptyFx fxs = Array.length fxs /= 0
