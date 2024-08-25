-- | FXs for the mixer
module Scene.Mixer.Fx
  ( withFxs
  , fxHtml
  , fxChannelSetup
  , sendChannelSetup
  , concatFxInfos
  ) where

import Prelude
import Effect (Effect)
import Scene.Mixer.Config
import Nexus.Ui.Core.Common as Ui
import Nexus.Ui.Core.Dial as Ui
import Nexus.Ui.Core.Button as Ui
import Scene.Html
import Data.Traversable (traverse)
import Data.Maybe (Maybe (..))
import Data.Array as Array
import Data.String as String
import Data.Tuple (Tuple (..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Action
  ( Mixer
  , ChannelId
  , FxId
  , FxParamId
  , SendId
  , SetFxParam
  , SetFxBypass
  , SetSendFx
  )
import Osc.Client (newOscControl)
import Common.Array (chunks)

fxRow :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
fxRow items =
  filledGrid maxChannelsPerRow items

maxChannelsPerRow :: Int
maxChannelsPerRow = 8

toFxHtml :: forall w i. String -> FxUi -> HH.HTML w i
toFxHtml channelName fx =
  HH.li []
    [ accordion fx.fx.name $ joinRows $
        Array.mapWithIndex (\n params -> fxRow (addBypassToFirst n $ map (fromFxParam channelName fx.fx.name) params)) (splitRows fx.fx.params)
    ]
  where
    addBypassToFirst n rows =
      (if (n == 0) then [bypassHtml] else [HH.div [] []]) <> rows

    splitRows =
      case fx.fx.unit of
        EqFxUnit -> toEqRows
        _ -> pure

    joinRows = case _ of
      [x] -> x
      xs -> HH.div [] xs

    bypassHtml =
      HH.div []
        [ divId (toBypassId channelName fx.fx.name) []
        , HH.text "bypass"
        ]


toEqRows :: forall a. Array a -> Array (Array a)
toEqRows params = chunks size params
  where
    size = Array.length params `div` 3

toChannelSendFxHtml :: forall w i. String -> Array SendFx -> HH.HTML w i
toChannelSendFxHtml channelName sends =
  HH.li [] [accordion "sends" $ HH.div [] (map (fromSendFx channelName) sends)]

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
    ["fx", "param", channelName, fxName, paramName]

toSendFxId :: String -> Int -> String
toSendFxId fromChannelName toChannel =
  String.joinWith "."
    ["sendFx", fromChannelName, show toChannel]

toBypassId :: String -> String -> String
toBypassId channelName fxName =
  String.joinWith "."
    ["fx", "bypass", channelName, fxName]

fxChannelSetup :: Mixer -> ChannelFxUi -> Effect FxInfo
fxChannelSetup act chan = do
  infos <- traverse (fxSetup act chan.name) chan.fxs
  pure $ concatFxInfos infos

concatFxInfos :: Array FxInfo -> FxInfo
concatFxInfos infos =
  { params: Array.concat $ map (_.params) infos
  , bypass: Array.concat $ map (_.bypass) infos
  }

sendChannelSetup :: Mixer -> ChannelFxUi -> Effect (Array (Tuple SendId SetSendFx))
sendChannelSetup act chan =
  traverse (\send -> sendFxSetup act chan.name chan.channelId send.to send.value) chan.sends

type FxInfo =
  { params :: Array (Tuple FxParamId SetFxParam)
  , bypass :: Array (Tuple FxId SetFxBypass)
  }

fxSetup :: Mixer -> String -> FxUi -> Effect FxInfo
fxSetup act channelName fx = do
  bypass <- map pure $ fxBypassSetup act fx.channel channelName fx.fx.name fx.fx.bypass
  params <- traverse (fxParamSetup act fx.channel channelName fx.fx.name) fx.fx.params
  pure { bypass, params }

fxBypassSetup :: Mixer -> Maybe ChannelId -> String -> String -> Boolean -> Effect (Tuple FxId SetFxBypass)
fxBypassSetup act channelId channelName fxName isBypass = do
  toggle <- Ui.newButtonBy ("#" <> toBypassId channelName fxName)
    { size: Tuple 75.0 75.0
    , mode: Ui.ToggleButton
    , state: isBypass
    }
  bypassOsc <- newOscControl $ \_ -> act.toggleFxBypass fxId
  toggle.on Ui.Change (bypassOsc.set unit)
  pure $
    Tuple
      fxId
      (\val -> bypassOsc.silent $ toggle.setState val)
  where
    fxId =
      { channel: channelId
      , name: fxName
      }


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
