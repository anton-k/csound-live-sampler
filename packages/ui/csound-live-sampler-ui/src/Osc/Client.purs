module Osc.Client
  ( OscConfig
  , OscClient
  , newOscEcho
  , newOscClient
  , SetListen
  , Clip (..)
  , newOscPort
  , readUiInfo
  , Control
  , newOscControl
  ) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Tuple (Tuple (..))
import Data.Tuple.Nested ((/\))
import Action
import Network.Osc as Osc
import Osc.Message as Osc
import Effect.Ref
import Data.Maybe
import Data.Int (round)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_, Canceler, makeAff, nonCanceler)
import Data.Either
import Scene.Config
import Effect.Exception (Error)
import Effect.Class (liftEffect)
import JSON as J

-- | Websocket address
type OscConfig =
  { address :: String
  , port :: Int
  }

type Listen =
  { bpm :: Int -> Effect Unit
  , masterVolumeEnvelope :: Number -> Effect Unit
  , masterVolume :: Number -> Effect Unit
  , channelVolumeEnvelope :: Int -> Number -> Effect Unit
  , channelVolume :: Int -> Number -> Effect Unit
  , channelMute :: Int -> Boolean -> Effect Unit
  , fxParam :: FxParamId -> Number -> Effect Unit
  , fxBypass :: FxId -> Boolean -> Effect Unit
  , sendFx :: SendId -> Number -> Effect Unit
  , partChange :: Clip -> Effect Unit
  }

emptyListen :: Listen
emptyListen =
  { bpm: const (pure unit)
  , masterVolumeEnvelope: const (pure unit)
  , masterVolume: const (pure unit)
  , channelVolumeEnvelope: const (const $ pure unit)
  , channelVolume: const (const $ pure unit)
  , channelMute: const (const $ pure unit)
  , fxParam: const (const $ pure unit)
  , fxBypass: const (const (pure unit))
  , sendFx: const (const (pure unit))
  , partChange: const (pure unit)
  }

type SetListen =
  { bpm :: (Int -> Effect Unit) -> Effect Unit
  , masterVolumeEnvelope :: (Number -> Effect Unit) -> Effect Unit
  , masterVolume :: (Number -> Effect Unit) -> Effect Unit
  , channelVolumeEnvelope :: (Int -> Number -> Effect Unit) -> Effect Unit
  , channelVolume :: (Int -> Number -> Effect Unit) -> Effect Unit
  , channelMute :: (Int -> Boolean -> Effect Unit) -> Effect Unit
  , fxParam :: (FxParamId -> Number -> Effect Unit) -> Effect Unit
  , fxBypass :: (FxId -> Boolean -> Effect Unit) -> Effect Unit
  , sendFx :: (SendId -> Number -> Effect Unit) -> Effect Unit
  , partChange :: (Clip -> Effect Unit) -> Effect Unit
  }

type OscClient  =
  { send :: Scene
  , listen :: SetListen
  }

newOscPort :: OscConfig -> Effect Osc.Port
newOscPort config =
  Osc.newWebsocketPort "ws://localhost:9090"-- (config.address <> ":" <> show config.port)

newOscClient :: Osc.Port -> Effect OscClient
newOscClient client = do
  listener <- new emptyListen
  runListener client listener
  pure $
    { send:
        { mixer: initMixer client
        , sampler: initSampler client
        , info: initInfo client
        }
    , listen: setListeners listener
    }

runListener :: Osc.Port -> Ref Listen -> Effect Unit
runListener port ref =
  port.listen $ \msg ->
    Osc.oscCase_ msg caseExpr onUnknown
  where
    onUnknown :: Osc.Osc -> Effect Unit
    onUnknown msg = log ("Unrecognized message: " <> show msg)

    caseExpr :: Array (Osc.OscCase (Effect Unit))
    caseExpr =
      [ Osc.toOscCase "/bpm/beats" onBpm
      , Osc.toOscCase "/master/volume/envelope" onMasterVolumeEnvelope
      , Osc.toOscCase "/master/volume/change" onMasterVolume
      , Osc.toOscCase "/channel/$d/volume/envelope" onChannelVolumeEnvelope
      , Osc.toOscCase "/channel/$d/volume/change" onChannelVolumeChange
      , Osc.toOscCase "/channel/$d/mute/change" onChannelMuteChange
      , Osc.toOscCase "/part/change" onPartChange
      , Osc.toOscCase "/channel/$d/fx/param/change/$s/$s" onChannelFxParamChange
      , Osc.toOscCase "/channel/$d/fx/bypass/change/$s" onChannelFxBypassChange
      , Osc.toOscCase "/channel/$d/send/change/$d" onChannelSendFxChange
      , Osc.toOscCase "/master/fx/param/change/$s/$s" onMasterFxParamChange
      , Osc.toOscCase "/master/fx/bypass/change/$s" onMasterFxBypassChange
      ]

    onBpm :: Int -> Effect Unit
    onBpm n = withListen $ \listen -> listen.bpm n

    onMasterVolumeEnvelope :: Number -> Effect Unit
    onMasterVolumeEnvelope volume =
      withListen $ \listen -> listen.masterVolumeEnvelope volume

    onMasterVolume :: Number -> Effect Unit
    onMasterVolume volume =
      withListen $ \listen -> listen.masterVolume volume

    onChannelVolumeEnvelope :: Tuple Number Number -> Effect Unit
    onChannelVolumeEnvelope (Tuple channelId volume) =
      withListen $ \listen -> listen.channelVolumeEnvelope (round channelId) volume

    onChannelVolumeChange :: Tuple Number Number -> Effect Unit
    onChannelVolumeChange (Tuple channelId volume) =
      withListen $ \listen -> listen.channelVolume (round channelId) volume

    onChannelMuteChange :: Tuple Number Number -> Effect Unit
    onChannelMuteChange (Tuple channelId flag) =
      withListen $ \listen -> listen.channelMute (round channelId) (round flag == 1)

    onChannelFxParamChange :: Tuple Number (Tuple String (Tuple String Number)) -> Effect Unit
    onChannelFxParamChange (Tuple channelId (Tuple name (Tuple param value))) =
      withListen $ \listen -> do
        let
          paramId =
            { channel: Just (round channelId)
            , name: name
            , param: param
            }
        listen.fxParam paramId value

    onChannelFxBypassChange :: Tuple Number (Tuple String Number)  -> Effect Unit
    onChannelFxBypassChange (Tuple channelId (Tuple name isBypassNumber)) =
      withListen $ \listen -> do
        let
          fxId =
            { channel: Just (round channelId)
            , name: name
            }
        listen.fxBypass fxId (numberToBoolean isBypassNumber)

    numberToBoolean x = round x == 1

    onChannelSendFxChange :: Tuple Number (Tuple Number Number) -> Effect Unit
    onChannelSendFxChange (Tuple fromChannelId (Tuple toChannelId value)) =
      withListen $ \listen -> do
        let
          sendId =
            { from: round fromChannelId
            , to: round toChannelId
            }
        listen.sendFx sendId value

    onMasterFxParamChange :: Tuple String (Tuple String Number) -> Effect Unit
    onMasterFxParamChange (Tuple name (Tuple param value)) =
      withListen $ \listen -> do
        let
          paramId =
            { channel: Nothing
            , name: name
            , param: param
            }
        listen.fxParam paramId value

    onMasterFxBypassChange :: Tuple String Number -> Effect Unit
    onMasterFxBypassChange (Tuple name isBypassNumber) =
      withListen $ \listen -> do
        let
          fxId =
            { channel: Nothing
            , name: name
            }
        listen.fxBypass fxId (numberToBoolean isBypassNumber)

    withListen :: (Listen -> Effect Unit) -> Effect Unit
    withListen cont = do
      listen <- read ref
      cont listen

    onPartChange :: Clip -> Effect Unit
    onPartChange (Clip val) =
      withListen $ \listen -> do
        log (show val)
        listen.partChange (Clip val)

newtype Clip = Clip
   { bpm :: Number
   , start :: Number
   , changeRate :: Number
   , beatSize :: Number
   , timeSize :: Number
   , measure :: Number
   , trackIndex :: Number
   , partIndex :: Number
   , numOfParts :: Number
   , nextAction :: Number
   }

derive newtype instance Show Clip

instance Osc.ReadOsc Clip where
  oscArity _ = 9
  readOsc =  case _ of
    [ Osc.OscDouble bpm
    , Osc.OscDouble start
    , Osc.OscDouble changeRate
    , Osc.OscDouble beatSize
    , Osc.OscDouble timeSize
    , Osc.OscDouble measure
    , Osc.OscDouble trackIndex
    , Osc.OscDouble partIndex
    , Osc.OscDouble numOfParts
    , Osc.OscDouble nextAction
    ] -> Just (Clip {bpm, start, changeRate, beatSize, timeSize, measure, trackIndex, partIndex, numOfParts, nextAction})
    _ -> Nothing

setListeners :: Ref Listen -> SetListen
setListeners ref =
  { bpm: \f -> modify_ (\s -> s { bpm = f }) ref
  , masterVolumeEnvelope: \f -> modify_ (\s -> s { masterVolumeEnvelope = f }) ref
  , masterVolume: \f -> modify_ (\s -> s { masterVolume = f }) ref
  , channelVolumeEnvelope: \f -> modify_ (\s -> s { channelVolumeEnvelope = f }) ref
  , channelVolume: \f -> modify_ (\s -> s { channelVolume = f }) ref
  , channelMute: \f -> modify_ (\s -> s { channelMute = f }) ref
  , partChange: \f -> modify_ (\s -> s { partChange = f }) ref
  , fxParam: \f -> modify_ (\s -> s { fxParam = f }) ref
  , fxBypass: \f -> modify_ (\s -> s { fxBypass = f }) ref
  , sendFx: \f -> modify_ (\s -> s { sendFx = f }) ref
  }

initMixer :: Osc.Port -> Mixer
initMixer port =
  { setMasterVolume: \volume -> port.send (Osc.setMasterVolume volume)
  , setChannelVolume: \chanId volume ->
      port.send (Osc.setChannelVolume chanId volume)
  , setFxParam: \paramId value -> do
      logValue "Send fx param" (paramId /\ value /\ Osc.setFxParam paramId value)
      port.send (Osc.setFxParam paramId value)
  , toggleFxBypass: \fxId -> do
      logValue "Send fx bypass" (fxId /\ Osc.toggleFxBypass fxId)
      port.send (Osc.toggleFxBypass fxId)
  , setSendFx: \sendId value -> do
      logValue "Send send fx" (sendId /\ value /\ Osc.setSendFx sendId value)
      port.send (Osc.setSendFx sendId value)
  }

initSampler :: Osc.Port -> Sampler
initSampler port =
  { setTrack: port.send <<< Osc.setTrack
  , shiftTrack: port.send <<< Osc.shiftTrack
  , shiftPart: port.send <<< Osc.shiftPart
  }

initInfo :: Osc.Port -> Info
initInfo port =
  { getCurrentPart: port.send Osc.getCurrentPart
  }

newOscEcho :: OscConfig -> Effect Scene
newOscEcho _config = do
  pure
    { mixer: initMixerEcho
    , sampler: initSamplerEcho
    , info: initInfoEcho
    }

initMixerEcho :: Mixer
initMixerEcho =
  { setMasterVolume: logValue "Master volume"
  , setChannelVolume: \chan vol -> logValue "Channel volume" (chan /\ vol)
  , setFxParam: \param value -> logValue "FX param" (param /\ value)
  , toggleFxBypass: \fxId -> logValue "FX bypass" fxId
  , setSendFx: \sendId value -> logValue "Send FX" (sendId /\ value)
  }

initSamplerEcho :: Sampler
initSamplerEcho =
  { setTrack: logValue "Set track"
  , shiftTrack: logValue "Shift track"
  , shiftPart: logValue "Shift part"
  }

initInfoEcho :: Info
initInfoEcho =
  { getCurrentPart: log "Get current track"
  }

logValue :: forall a . Show a => String -> a -> Effect Unit
logValue title value = log $ title <> ": " <> show value

readUiInfo :: Osc.Port -> Aff (Either String SceneUi)
readUiInfo oscPort = map toResult $ makeAff go
  where
    go :: (Either Error String -> Effect Unit) -> Effect Canceler
    go cont = do
      liftEffect $ oscPort.listen $ \msg -> do
        Osc.oscCase_ msg
          [ Osc.toOscCase "/ui/info/put" (cont <<< Right)
          ] (const $ pure unit)
      liftEffect $ oscPort.send { address: "/ui/info/get", args: [] }
      pure $ nonCanceler

    toResult :: String -> Either String SceneUi
    toResult str = do
      json <- J.parse str
      note (errMNessage json) (sceneUiFromJson json)
      where
        errMNessage js = "Failed to parswe SceneUi from JSON: " <> J.printIndented js

type Control a =
  { set :: a -> Effect Unit
  , silent :: Effect Unit -> Effect Unit
  }

newOscControl :: forall a . (a -> Effect Unit) -> Effect (Control a)
newOscControl setter = do
  isActiveRef <- new true
  pure $
    { set: \value -> do
        isActive <- read isActiveRef
        when isActive $ setter value
    , silent: \act -> do
        write false isActiveRef
        res <- act
        write true isActiveRef
        pure res
    }
