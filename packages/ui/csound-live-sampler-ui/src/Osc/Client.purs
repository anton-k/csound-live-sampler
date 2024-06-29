module Osc.Client
  ( OscConfig
  , newOscEcho
  , newOscClient
  ) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Tuple.Nested ((/\))
import Action
import Network.Osc as Osc
import Osc.Message as Osc

type OscConfig =
  { address :: String
  , port :: Int
  }

newOscClient :: OscConfig -> Effect Scene
newOscClient _config = do
  client <- Osc.newWebsocketPort "ws://localhost:9090"
  pure
    { mixer: initMixer client
    , sampler: initSampler client
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
