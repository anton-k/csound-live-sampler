module Scene.AudioCard
  ( InitAudioCard
  , initAudioCard
  ) where

import Prelude
import Effect (Effect)
import Scene.Mixer.Config
import Nexus.Ui.Core.Common as Ui
import Nexus.Ui.Core.Dial as Ui
import Scene.Html
import Data.Traversable (traverse, traverse_)
import Data.Maybe (Maybe (..))
import Data.Array as Array
import Data.String as String
import Data.Tuple (Tuple (..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Action (SetAudioCard)
import Osc.Client (newOscControl)
import Common.Array (chunks)
import Scene.AudioCard.Config
import Data.Map (Map)
import Data.Map as Map

type InitAudioCard =
  { addHtml :: forall w i. HH.HTML w i -> HH.HTML w i
  , setup :: Effect SetAudioCard
  }

initAudioCard :: AudioCardUi -> InitAudioCard
initAudioCard config =
  { addHtml: withAudioCard config
  , setup: setupAudioCard config
  }

withAudioCard :: forall w i. AudioCardUi -> HH.HTML w i -> HH.HTML w i
withAudioCard config body
  | Array.null config.inputs = body
  | otherwise =
      HH.div []
        [ body
        , audioCardHtml config.inputs
        ]

audioCardHtml :: forall w i. Array Int -> HH.HTML w i
audioCardHtml inputs =
  accordion "Audio card" $
    divClasses ["grid"] (map inputHtml inputs)

inputHtml :: forall w i. Int -> HH.HTML w i
inputHtml channelId = HH.div []
  [ inputGain
  -- , inputMute channelId -- TODO: add muters on the audio engine level
  , inputName
  ]
  where
    inputName = HH.div [] [HH.text (show channelId)]

    inputGain = divId (toAudioCardId channelId) []

toAudioCardId :: Int -> String
toAudioCardId inputId = "audioCardInput" <> show inputId

setupAudioCard :: AudioCardUi -> Effect SetAudioCard
setupAudioCard config = do
  setterMap <- map Map.fromFoldable $
    traverse (\inputId -> map (Tuple inputId) $ setupAudioInput inputId) config.inputs
  pure $
    { setGain: \inputId value ->
        traverse_ (_ $ value) $ Map.lookup inputId setterMap
    }

setupAudioInput :: Int -> Effect (Number -> Effect Unit)
setupAudioInput _inputId = pure $ todo
  where
    -- TODO
    todo = const (pure unit)
