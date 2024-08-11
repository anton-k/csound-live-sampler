module Scene.Mixer
  ( SetMixer
  , SetMaster
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
import Data.Traversable (traverse_, traverse)
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
import Scene.Mixer.Fx (withFxs, fxHtml, fxChannelSetup, SetFxParam)

type SetMixer =
  { setMaster :: SetMaster
  , setChannel :: Int -> SetChannel
  , setFxParam :: FxParamId -> SetFxParam
  }

type SetChannel =
  { setVolumeEnvelope :: Number -> Effect Unit
  , setVolume :: Number -> Effect Unit
  , setMute :: Boolean -> Effect Unit
  }

type SetMaster =
  { setVolumeEnvelope :: Number -> Effect Unit
  , setVolume :: Number -> Effect Unit
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

emptySetFxParam :: SetFxParam
emptySetFxParam = const (pure unit)

initMixer :: forall w s . MixerUi -> Mixer -> Elem w s SetMixer
initMixer mixer act =
  { setup: do
      setMaster <- master.setup
      channelMap <-
        map Map.fromFoldable $
          traverseWithIndex
            (\index item -> map (Tuple (index + 1)) item.setup)
            channels
      let
        getChannelSetters chanId =
          fromMaybe emptySetChannel (Map.lookup chanId channelMap)
      fxParamMap <- map (Map.fromFoldable <<< Array.concat) $
        traverse (fxChannelSetup act) fxs
      let
        getFxParamSetter paramId =
          fromMaybe emptySetFxParam (Map.lookup paramId fxParamMap)
      pure
        { setChannel: getChannelSetters
        , setFxParam: getFxParamSetter
        , setMaster: setMaster
        }
  , html:
      withFxs fxs $
        divClasses ["grid"]
          ( map (\item -> toColumn item.html) channels <>
            [toColumn master.html]
          )
  }
  where
    channels = map (initChannel act) mixer.channels
    master = initMasterChannel act mixer.master

    toColumn x = divClasses [] [x]

    fxs = toMixerFxUis mixer

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
        ]
  }
  where
    barTarget = "bar" <> show item.channel
    dialTarget = "dial" <> show item.channel

initMasterChannel :: forall w s . Mixer -> MasterUiItem -> Elem w s SetMaster
initMasterChannel act item =
  { setup: do
      dial <- Ui.newDial ("#" <> dialTarget)
      dial.setValue item.volume
      bar <- initBar ("#" <> barTarget) 1 0.0
      sendOscVolume <- newOscControl $ \val ->
          act.setMasterVolume val
      dial.on Ui.Change sendOscVolume.set

      pure $
        { setVolumeEnvelope: \volume -> bar.setAllSliders [volume]
        , setVolume: \volume -> do
            sendOscVolume.silent $ dial.setValue volume
        }

  , html:
      divClasses []
        [ divId barTarget []
        , divId "space1" [HH.p_ [HH.text " "]]
        , divId dialTarget []
        , divId "space1" [HH.p_ [HH.text " "]]
        , case (Just "main") of
              Just name -> HH.div [HP.style "text-align:center"] [HH.text name]
              Nothing -> divClasses [] []
        ]
  }
  where
    barTarget = "bar" <> masterTag
    dialTarget = "dial" <> masterTag
    masterTag = "masterChannel"


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
