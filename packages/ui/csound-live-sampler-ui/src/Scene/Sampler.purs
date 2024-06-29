module Scene.Sampler
  ( SamplerUi
  , SetSampler
  , Track
  , initSampler
  ) where

import Prelude
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Scene.Elem
import Scene.Html
import Data.Maybe
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Data.Array as Array
import DOM.HTML.Indexed.InputType (InputType (..))
import Data.Foldable
import Nexus.Ui.General.RadioButton
import Data.Int (toNumber)

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

type SetSampler =
  { setBpm :: Int -> Effect Unit
  }

initSampler :: forall a b . SamplerUi -> Elem a b SetSampler
initSampler sampler =
  { setup: do
      bpm <- newRadioButtonBy "#bpm"
        { size: (25.0 * 1.15 * toNumber sampler.measure) /\ 25.0
        , numberOfButtons: sampler.measure
        , active: 0
        }
      pure
        { setBpm: bpm.select
        }

  , html:

      HH.nav_ $ map (\items -> HH.ul_ (map (HH.li_ <<< pure) items))
        [ [bpmLights sampler.measure]
        , [ tracks, prev, next, sep, sizes, prev, next ]
        ]
  }
  where
    bpmLights _n = divId "bpm" []

    tracks =
      HH.select [HP.name "select", HP.required true] $
        [HH.option [HP.selected true, HP.disabled true, HP.value ""] [strongText "Track"]]
        <> (map (\track -> HH.option_ [strongText track.name]) sampler.tracks)

    sizes = strongText "1 : 6"
    sep = strongText "|"

    prev = HH.a_ [strongText "<="]
    next = HH.a_ [strongText "=>"]

    strongText = HH.strong_ <<< pure <<< HH.text

toBpmName :: Int -> String
toBpmName index = "bpm-" <> show index
