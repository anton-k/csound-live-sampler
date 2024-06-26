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
import Halogen.HTML.Properties as HP
import Data.Array as Array
import DOM.HTML.Indexed.InputType (InputType (..))

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
initSampler sampler =
  { setup: pure unit
  , html:

      HH.nav_ $ map (\items -> HH.ul_ (map (HH.li_ <<< pure) items))
        [ [bpmLights sampler.measure]
        , [ tracks, prev, next, sep, sizes, prev, next ]
        ]
  }
  where
    bpmLights n =
      HH.fieldset_ (toBpmLight =<< (Array.range 1 n))

    toBpmLight index =
      [ HH.input [HP.type_ InputRadio, HP.id name, HP.name "bpm", HP.checked (index == 0)]
--      , HH.label [HP.htmlFor name] [HH.text " "]
      ]
      where
        name = "bpm-" <> show index


    tracks =
      HH.select [HP.name "select", HP.required true] $
        [HH.option [HP.selected true, HP.disabled true, HP.value ""] [strongText "Track"]]
        <> (map (\track -> HH.option_ [strongText track.name]) sampler.tracks)

    sizes = strongText "1 : 6"
    sep = strongText "|"

    prev = HH.a_ [strongText "<="]
    next = HH.a_ [strongText "=>"]

    strongText = HH.strong_ <<< pure <<< HH.text
