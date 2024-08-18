module Scene.Config
  ( SceneUi
  , sceneUiFromJson
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import JSON (JSON)
import JSON as Json
import JSON.Object as Json
import Data.Traversable (traverse)
import Common.JSON.Extra (lookupArray, lookupString, lookupNumber, lookupInt)
import Scene.Mixer.Config (MixerUi, mixerUiFromJson)
import Scene.Sampler.Config (SamplerUi, samplerUiFromJson)

type SceneUi =
  { mixer :: MixerUi
  , sampler :: SamplerUi
  }

sceneUiFromJson :: JSON -> Maybe SceneUi
sceneUiFromJson json = do
  obj <- Json.toJObject json
  mixer <- mixerUiFromJson =<< Json.lookup "mixer" obj
  sampler <- samplerUiFromJson =<< Json.lookup "sampler" obj
  pure { mixer, sampler }

