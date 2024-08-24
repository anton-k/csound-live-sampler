module Scene.AudioCard.Config
  ( AudioCardUi
  , audioCardUiFromJson
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import JSON (JSON)
import JSON as Json
import JSON.Object as Json
import Data.Traversable (traverse)
import Common.JSON.Extra (lookupArray, lookupString, lookupNumber, lookupInt)
import Data.Int (round)

type AudioCardUi =
  { inputs :: Array Int
  }

audioCardUiFromJson :: JSON -> Maybe AudioCardUi
audioCardUiFromJson json = do
  obj <- Json.toJObject json
  inputs <- traverse (map round <<< Json.toNumber) =<< lookupArray "inputs" obj
  pure { inputs }
