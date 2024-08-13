-- | FXs for the mixer
module Scene.Mixer.Fx
  ( withFxs
  , fxHtml
  , fxChannelSetup
  , sendChannelSetup
  , SetFxParam
  , SetSendFx
  ) where

import Prelude
import Effect (Effect)
import Scene.Mixer.Config
import Nexus.Ui.Core.Common as Ui
import Nexus.Ui.Core.Dial as Ui
import Scene.Html
import Data.Traversable (traverse)
import Data.Maybe (Maybe (..))
import Data.Array as Array
import Data.String as String
import Data.Tuple (Tuple (..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Action (Mixer, ChannelId, FxParamId, SendId)
import Osc.Client (newOscControl)

fxRow :: forall w i. String -> Array (HH.HTML w i) -> HH.HTML w i
fxRow name items =
  HH.li [] [divClasses ["grid"] ([HH.div [] [HH.text name]] <> items)]

toFxHtml :: forall w i. String -> FxUi -> HH.HTML w i
toFxHtml channelName fx =
  fxRow fx.fx.name (map (fromFxParam channelName fx.fx.name) fx.fx.params)

toChannelSendFxHtml :: forall w i. String -> Array SendFx -> HH.HTML w i
toChannelSendFxHtml channelName sends =
  fxRow "sends" (map (fromSendFx channelName) sends)

fromFxParam :: forall w i. String -> String -> FxParam -> HH.HTML w i
fromFxParam channelName fxName param =
  HH.div []
    [ divId (toFxId channelName fxName param.name) []
    , HH.div [HP.style "text-align:left"] [HH.text param.name]
    ]

fromSendFx :: forall w i. String -> SendFx -> HH.HTML w i
fromSendFx channelName send =
  HH.div []
    [ divId (toSendFxId channelName send.to) []
    , HH.div [HP.style "text-align:left"] [HH.text (show send.to)]
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
    HH.ul [] (sendFx <> map (toFxHtml chan.name) chan.fxs)
  where
    sendFx
      | Array.null chan.sends = []
      | otherwise = [toChannelSendFxHtml chan.name chan.sends]

toFxId :: String -> String -> String -> String
toFxId channelName fxName paramName =
  String.joinWith "."
    ["fx", channelName, fxName, paramName]

toSendFxId :: String -> Int -> String
toSendFxId fromChannelName toChannel =
  String.joinWith "."
    ["sendFx", fromChannelName, show toChannel]

fxChannelSetup :: Mixer -> ChannelFxUi -> Effect (Array (Tuple FxParamId SetFxParam))
fxChannelSetup act chan =
  map Array.concat $ traverse (fxSetup act chan.name) chan.fxs

sendChannelSetup :: Mixer -> ChannelFxUi -> Effect (Array (Tuple SendId SetSendFx))
sendChannelSetup act chan =
  traverse (\send -> sendFxSetup act chan.name chan.channelId send.to send.value) chan.sends

fxSetup :: Mixer -> String -> FxUi -> Effect (Array (Tuple FxParamId SetFxParam))
fxSetup act channelName fx =
  traverse (fxParamSetup act fx.channel channelName fx.fx.name) fx.fx.params

type SetFxParam = Number -> Effect Unit

fxParamSetup :: Mixer -> Maybe ChannelId -> String -> String -> FxParam -> Effect (Tuple FxParamId SetFxParam)
fxParamSetup act channelId channelName fxName param = do
  dial <- Ui.newDial ("#" <> toFxId channelName fxName param.name)
  dial.setValue param.value
  sendOscFxParam <- newOscControl $ \val -> act.setFxParam paramId val
  dial.on Ui.Change sendOscFxParam.set
  pure $
    Tuple
      paramId
      (\value -> sendOscFxParam.silent $ dial.setValue value)
  where
    paramId =
      { channel: channelId
      , name: fxName
      , param: param.name
      }

type SetSendFx = Number -> Effect Unit

sendFxSetup :: Mixer -> String -> ChannelId -> ChannelId -> Number -> Effect (Tuple SendId SetSendFx)
sendFxSetup act channelName fromChannel toChannel value = do
  dial <- Ui.newDial ("#" <> toSendFxId channelName toChannel)
  dial.setValue value
  sendOscSend <- newOscControl $ \val -> act.setSendFx sendId val
  dial.on Ui.Change sendOscSend.set
  pure $
    Tuple
      sendId
      (\value -> sendOscSend.silent $ dial.setValue value)
  where
    sendId = { from: fromChannel, to: toChannel }
