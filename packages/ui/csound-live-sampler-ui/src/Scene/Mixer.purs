module Scene.Mixer
  ( SetMixer
  , SetChannel
  , initMixer
  , initChannel
  , module Scene.Mixer.Config
  ) where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Nexus.Ui.Core.Common as Ui
import Nexus.Ui.Core.Dial as Ui
import Nexus.Ui.General.Multislider as Ui
import Effect.Console (log)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Hooks as Hooks
import Data.Tuple (Tuple (..))
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..), fromMaybe)
import Control.Monad.Trans.Class (class MonadTrans)
import Effect.Aff (Aff)
import Data.Traversable (traverse_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Array (range)
import Scene.Elem
import Scene.Html
import Data.Array as Array
import Data.String as String
import Action
import Data.Map (Map)
import Data.Map as Map
import Effect.Ref (new, read, write)
import Scene.Mixer.Config
import Osc.Client (newOscControl)

type SetMixer =
  { setChannel :: Int -> SetChannel
  }

type SetChannel =
  { setVolumeEnvelope :: Number -> Effect Unit
  , setVolume :: Number -> Effect Unit
  , setMute :: Boolean -> Effect Unit
  }

defColor0 = "#24bcbc"
defColor1 = "#66c8c8"
defColor2 = "#bbdcdc"
defColor3 = "#9cd8d8"
defColor4 = "#dcecf0"

emptySetChannel :: SetChannel
emptySetChannel =
  { setVolumeEnvelope: const (pure unit)
  , setVolume: const (pure unit)
  , setMute: const (pure unit)
  }

initMixer :: forall w s . MixerUi -> Mixer -> Elem w s SetMixer
initMixer mixer act =
  { setup: do
      channelMap <-
        map Map.fromFoldable $
          traverseWithIndex
            (\index item -> map (Tuple (index + 1)) item.setup)
            channels
      let
        getChannelSetters chanId =
          fromMaybe emptySetChannel (Map.lookup chanId channelMap)
      traverse_ fxChannelSetup fxs
      pure
        { setChannel: getChannelSetters
        }
  , html:
      withFxs fxs $
        divClasses ["grid"] (map (\item -> toColumn item.html) channels)
  }
  where
    channels = map (initChannel act) mixer.channels

    toColumn x = divClasses [] [x]

    fxs = toChannelFxUis mixer

initChannel :: forall w s . Mixer -> MixerUiItem -> Elem w s SetChannel
initChannel act item =
  { setup: do
      dial <- Ui.newDial ("#" <> dialTarget)
      dial.setValue item.volume
      bar <- initBar ("#" <> barTarget) item.channel 0.0
      sendOscVolume <- newOscControl $ \val ->
          act.setChannelVolume item.channel val
      dial.on Ui.Change sendOscVolume.set

      pure $
        { setVolumeEnvelope: \volume -> bar.setAllSliders [volume]
        , setVolume: \volume -> do
            sendOscVolume.silent $ dial.setValue volume
        , setMute: \isMute ->
            if isMute
              then do
                bar.colorize Ui.AccentColor "#AAAAAA"
                dial.colorize Ui.AccentColor "#AAAAAA"
              else do
                bar.colorize Ui.AccentColor defColor0
                dial.colorize Ui.AccentColor defColor0
        }

  , html:
      divClasses []
        [ divId barTarget []
        , divId "space1" [HH.p_ [HH.text " "]]
        , divId dialTarget []
        , divId "space1" [HH.p_ [HH.text " "]]
        , case item.name of
              Just name -> HH.div [HP.style "text-align:center"] [HH.text name]
              Nothing -> divClasses [] []
        -- TODO: display FXs with accordeon
        -- , if not (Array.null item.fxs) then button else divClasses [] []
        ]
  }
  where
    barTarget = "bar" <> show item.channel
    dialTarget = "dial" <> show item.channel
    button = textButton "FX"

initBar :: String -> Int -> Number -> Effect Ui.Multislider
initBar target n initValue = do
  bar <- Ui.newMultisliderBy target
    { size: 60.0 /\ 300.0
    , min: 0.0
    , max: 1.0
    , step: 0.0
    , values: [initValue]
    , candycane: 3.0
    , smoothing: 0.0
    , mode: Ui.BarMultislider
    , numberOfSliders: 1
    }
  bar.on Ui.Change (\val -> log ("Slider" <> show n <> ": " <> show val))
  pure bar

-- * FXs

toFxHtml :: forall w i. String -> Fx -> HH.HTML w i
toFxHtml channelName fx = HH.li []
  [ divClasses ["grid"] $
      [HH.div [] [HH.text fx.name]] <> map (fromFxParam channelName fx.name) fx.params
  ]

fromFxParam :: forall w i. String -> String -> FxParam -> HH.HTML w i
fromFxParam channelName fxName param =
  HH.div []
    [ divId (toFxId channelName fxName param.name) []
    , HH.div [HP.style "text-align:left"] [HH.text param.name]
    ]

withFxs :: forall w i. Array ChannelFxUi -> HH.HTML w i -> HH.HTML w i
withFxs fxs x
  | Array.null fxs = x
  | otherwise =
      HH.div []
        [ x
        , HH.div [] [HH.p [] [HH.text "  "]]
        , HH.div [] [HH.p [] [HH.text "  "]]
        , fxHtml fxs
        ]

fxHtml :: forall w i. Array ChannelFxUi -> HH.HTML w i
fxHtml fxs =
  accordion "FXs" $
    HH.div [] (map toChannelFxHtml fxs)

toChannelFxHtml :: forall w i. ChannelFxUi -> HH.HTML w i
toChannelFxHtml chan =
  accordion chan.name $
    HH.ul [] (map (toFxHtml chan.name) chan.fxs)

toFxId :: String -> String -> String -> String
toFxId channelName fxName paramName =
  String.joinWith "."
    ["fx", channelName, fxName, paramName]

fxChannelSetup :: ChannelFxUi -> Effect Unit
fxChannelSetup chan =
  traverse_ (fxSetup chan.name) chan.fxs

fxSetup :: String -> Fx -> Effect Unit
fxSetup channelName fx =
  traverse_ (fxParamSetup channelName fx.name) fx.params

fxParamSetup :: String -> String -> FxParam -> Effect Unit
fxParamSetup channelName fxName param = do
  dial <- Ui.newDial ("#" <> toFxId channelName fxName param.name)
  dial.setValue param.value
