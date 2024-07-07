module Scene.Mixer.Config
  ( MixerUi
  , MixerUiItem
  , Fx
  , FxParam
  , mixerUiFromJson
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import JSON (JSON)
import JSON as Json
import JSON.Object as Json
import Data.Int (round)
import Data.Traversable (traverse)
import JSON.Extra (lookupArray, lookupString, lookupNumber, lookupInt)

type MixerUi =
  { channels :: Array MixerUiItem
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
  pure { channels }

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
