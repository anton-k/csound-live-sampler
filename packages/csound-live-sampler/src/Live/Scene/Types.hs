module Live.Scene.Types (
  Scene (..),
) where

import Live.Scene.AudioCard
import Live.Scene.Mixer
import Live.Scene.Sampler

data Scene = Scene
  { mixer :: Mixer
  , sampler :: Sampler
  , audio :: AudioCard
  }
