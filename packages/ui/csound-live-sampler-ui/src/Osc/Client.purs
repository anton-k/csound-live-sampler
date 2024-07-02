module Osc.Client
  ( OscConfig
  , OscClient
  , newOscEcho
  , newOscClient
  , SetListen
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

type OscConfig =
  { address :: String
  , port :: Int
  }

type Listen =
  { bpm :: Int -> Effect Unit
  , channelVolumeEnvelope :: Int -> Number -> Effect Unit
  , channelVolume :: Int -> Number -> Effect Unit
  , channelMute :: Int -> Boolean -> Effect Unit
  }

emptyListen :: Listen
emptyListen =
  { bpm: const (pure unit)
  , channelVolumeEnvelope: const (const $ pure unit)
  , channelVolume: const (const $ pure unit)
  , channelMute: const (const $ pure unit)
  }

type SetListen =
  { bpm :: (Int -> Effect Unit) -> Effect Unit
  , channelVolumeEnvelope :: (Int -> Number -> Effect Unit) -> Effect Unit
  , channelVolume :: (Int -> Number -> Effect Unit) -> Effect Unit
  , channelMute :: (Int -> Boolean -> Effect Unit) -> Effect Unit
  }

type OscClient  =
  { send :: Scene
  , listen :: SetListen
  }

newOscClient :: OscConfig -> Effect OscClient
newOscClient _config = do
  client <- Osc.newWebsocketPort "ws://localhost:9090"
  listener <- new emptyListen
  runListener client listener
  pure $
    { send:
        { mixer: initMixer client
        , sampler: initSampler client
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
      , Osc.toOscCase "/channel/$d/volume/envelope" onChannelVolumeEnvelope
      , Osc.toOscCase "/channel/$d/volume/change" onChannelVolumeChange
      , Osc.toOscCase "/channel/$d/mute/change" onChannelMuteChange
      ]

    onBpm :: Int -> Effect Unit
    onBpm n = withListen $ \listen -> listen.bpm n

    onChannelVolumeEnvelope :: Tuple Number Number -> Effect Unit
    onChannelVolumeEnvelope (Tuple channelId volume) =
      withListen $ \listen -> listen.channelVolumeEnvelope (round channelId) volume

    onChannelVolumeChange :: Tuple Number Number -> Effect Unit
    onChannelVolumeChange (Tuple channelId volume) =
      withListen $ \listen -> listen.channelVolume (round channelId) volume

    onChannelMuteChange :: Tuple Number Number -> Effect Unit
    onChannelMuteChange (Tuple channelId flag) =
      withListen $ \listen -> listen.channelMute (round channelId) (round flag == 1)

    withListen :: (Listen -> Effect Unit) -> Effect Unit
    withListen cont = do
      listen <- read ref
      cont listen

setListeners :: Ref Listen -> SetListen
setListeners ref =
  { bpm: \f -> modify_ (\s -> s { bpm = f }) ref
  , channelVolumeEnvelope: \f -> modify_ (\s -> s { channelVolumeEnvelope = f }) ref
  , channelVolume: \f -> modify_ (\s -> s { channelVolume = f }) ref
  , channelMute: \f -> modify_ (\s -> s { channelMute = f }) ref
  }

initMixer :: Osc.Port -> Mixer
initMixer port =
  { setMasterVolume: \volume -> port.send (Osc.setMasterVolume volume)
  , setChannelVolume: \chanId volume -> port.send (Osc.setChannelVolume chanId volume)
  }

initSampler :: Osc.Port -> Sampler
initSampler port =
  { setTrack: port.send <<< Osc.setTrack
  , shiftTrack: port.send <<< Osc.shiftTrack
  , shiftPart: port.send <<< Osc.shiftPart
  }

newOscEcho :: OscConfig -> Effect Scene
newOscEcho _config = do
  pure
    { mixer: initMixerEcho
    , sampler: initSamplerEcho
    }

initMixerEcho :: Mixer
initMixerEcho =
  { setMasterVolume: logValue "Master volume"
  , setChannelVolume: \chan vol -> logValue "Channel volume" (chan /\ vol)
  }

initSamplerEcho :: Sampler
initSamplerEcho =
  { setTrack: logValue "Set track"
  , shiftTrack: logValue "Shift track"
  , shiftPart: logValue "Shift part"
  }

logValue :: forall a . Show a => String -> a -> Effect Unit
logValue title value = log $ title <> ": " <> show value
