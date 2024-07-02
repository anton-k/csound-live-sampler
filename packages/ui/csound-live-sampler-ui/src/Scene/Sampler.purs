module Scene.Sampler
  ( SamplerUi
  , SetSampler
  , Track
  , initSampler
  ) where

import Prelude
import Data.Tuple (Tuple (..))
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
import Nexus.Ui.General.TextButton
import Nexus.Ui.Core.Common
import Data.Int (toNumber)
import Action

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

initSampler :: forall a b . SamplerUi -> Sampler -> Elem a b SetSampler
initSampler sampler act =
  { setup: do
      bpm <- newRadioButtonBy "#bpm"
        { size: (25.0 * 1.15 * toNumber sampler.measure) /\ 25.0
        , numberOfButtons: sampler.measure
        , active: 0
        }
      prevTrack <- navButton "#prevTrack" "<<="
      prevTrack.on Change $ onButtonOn $ act.shiftTrack (-1)
      nextTrack <- navButton "#nextTrack" "=>>"
      nextTrack.on Change $ onButtonOn $ act.shiftTrack 1

      prevPart <- navButton "#prevPart" "<<"
      prevPart.on Change $ onButtonOn $ act.shiftPart (-1)
      nextPart <- navButton "#nextPart" ">>"
      nextPart.on Change $ onButtonOn $ act.shiftPart 1
      _partLabel <- navButton "#partLabel" "1 : 4"

      pure
        { setBpm: bpm.select
        }

  , html:

      HH.nav_ $ map (\items -> HH.ul_ (map (HH.li_ <<< pure) items))
        [ [bpmLights]
        , [ tracks
          , divId "prevTrack" [], divId "nextTrack" []
          , sep
          -- , HH.div_ [sizes]
          , HH.div_ [strongText "Part:"]
          , divId "partLabel" []
          , divId "prevPart" [], divId "nextPart" []
          ]
        ]
  }
  where
    bpmLights = divId "bpm" []

    tracks =
      HH.select [HP.name "select", HP.required true] $
        [HH.option [HP.selected true, HP.disabled true, HP.value ""] [strongText "Track"]]
        <> (Array.concat $ Array.replicate 10 $ map (\track -> HH.option_ [strongText track.name]) sampler.tracks)

    sizes = strongText "1 : 6"
    sep = strongText "|"

    prev = HH.a_ [strongText "<="]
    next = HH.a_ [strongText "=>"]

    strongText = HH.strong_ <<< pure <<< HH.text

toBpmName :: Int -> String
toBpmName index = "bpm-" <> show index

onButtonOn :: Effect Unit -> Boolean -> Effect Unit
onButtonOn act flag =
  when flag act

navButton :: String -> String -> Effect TextButton
navButton target name = newTextButtonBy target
  { size: Tuple 60.0 40.0
  , state: false
  , text: name
  , alternateText: Nothing
  }
