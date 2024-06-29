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
import Data.Tuple.Nested ((/\))
import Action
import Network.Osc as Osc
import Osc.Message as Osc
import Effect.Ref

type OscConfig =
  { address :: String
  , port :: Int
  }

type Listen =
  { bpm :: Int -> Effect Unit
  }

emptyListen :: Listen
emptyListen =
  { bpm: const (pure unit)
  }

type SetListen =
  { bpm :: (Int -> Effect Unit) -> Effect Unit
  }

type OscClient  =
  { send :: Scene
  , listen :: SetListen
  }

newOscClient :: OscConfig -> Effect OscClient
newOscClient _config = do
  client <- Osc.newWebsocketPort "ws://localhost:9090"
  listener <- new emptyListen
  pure $
    { send:
        { mixer: initMixer client
        , sampler: initSampler client
        }
    , listen: setListeners listener
    }

setListeners :: Ref Listen -> SetListen
setListeners ref =
  { bpm: \f -> modify_ (\s -> s { bpm = f }) ref
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
