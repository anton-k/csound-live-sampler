module Scene.Sampler
  ( SamplerUi
  , Track
  , initSampler
  ) where

import Prelude
import Scene.Elem
import Scene.Html
import Data.Maybe
import Halogen.HTML as HH

type SamplerUi =
  { tracks :: Array Track
  , current :: Maybe Int
  , bpm :: Number
  , measure :: Int
  }

type Track =
  { name :: String
  , size :: Int
  , current :: Int
  }

initSampler :: forall a b . SamplerUi -> Elem a b
initSampler _sampler =
  { setup: pure unit
  , html:
      divClasses ["grid"]
        [ HH.div_ [bpm]
        , HH.div_ [tracks]
        , HH.div_ [sizes]
        ]
  }
  where
    bpm = HH.text "Bpm"
    tracks = HH.text "Tracks"
    sizes = HH.text "Sizes"

