module Scene.Sampler.Config
  ( SamplerUi
  , TrackUi
  , samplerUiFromJson
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import JSON (JSON)
import JSON as Json
import JSON.Object as Json
import Data.Traversable (traverse)
import Common.JSON.Extra (lookupArray, lookupString, lookupNumber, lookupInt)

type SamplerUi =
  { tracks :: Array TrackUi
  }

type TrackUi =
  { name :: String
  , size :: Int
  }

-- From JSON

samplerUiFromJson :: JSON -> Maybe SamplerUi
samplerUiFromJson json = do
  obj <- Json.toJObject json
  tracks <- traverse trackUiFromJson =<< lookupArray "tracks" obj
  pure { tracks }

trackUiFromJson :: JSON -> Maybe TrackUi
trackUiFromJson json = do
  obj <- Json.toJObject json
  name <- lookupString "name" obj
  size <- lookupInt "size" obj
  pure { name, size }

