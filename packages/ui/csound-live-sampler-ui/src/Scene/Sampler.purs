module Scene.Sampler
  ( SamplerUi
  , Track
  , initSampler
  , initSetSamplerUi
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
import Web.DOM.NonElementParentNode as Dom
import Web.DOM.Element as Dom
import Data.Foldable
import Web.HTML as Dom
import Web.DOM.Document as Dom
import Web.HTML.HTMLDocument as Html
import Web.HTML.Window as Html
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

type SetSamplerUi =
  { setBpm :: Int -> Effect Unit
  }

initSetSamplerUi :: SamplerUi -> SetSamplerUi
initSetSamplerUi config =
  { setBpm: \index -> do
      traverse_ clearChecked (Array.range 1 config.measure)
      setCheck index
  }
  where
    clearChecked index = do
      doc <- map (Dom.toNonElementParentNode <<< Html.toDocument) (Html.document =<< Dom.window)
      mElem <- Dom.getElementById (toBpmName index) doc
      traverse_ (Dom.removeAttribute "checked") mElem

    setCheck index = do
      doc <- map (Dom.toNonElementParentNode <<< Html.toDocument) (Html.document =<< Dom.window)
      mElem <- Dom.getElementById (toBpmName index) doc
      traverse_ (Dom.setAttribute "checked" "checked") mElem


initSampler :: forall a b . SamplerUi -> Elem a b
initSampler sampler =
  { setup: do
      void $ newRadioButtonBy "#bpm"
        { size: (25.0 * 1.15 * toNumber sampler.measure) /\ 25.0
        , numberOfButtons: sampler.measure
        , active: 0
        }

  , html:

      HH.nav_ $ map (\items -> HH.ul_ (map (HH.li_ <<< pure) items))
        [ [bpmLights sampler.measure]
        , [ tracks, prev, next, sep, sizes, prev, next ]
        ]
  }
  where
    bpmLights _n = divId "bpm" []
{-
      HH.fieldset_ (toBpmLight =<< (Array.range 1 n))

    toBpmLight index =
      [  HH.input [HP.type_ InputRadio, HP.id (toBpmName index), HP.name "bpm", HP.checked (index == 1)]
--      , HH.label [HP.htmlFor name] [HH.text " "]
      ]
-}

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
