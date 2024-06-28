module Osc.Client
  ( OscConfig
  , newOscEcho
  ) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Tuple.Nested ((/\))
import Action

type OscConfig =
  { address :: String
  , port :: Int
  }

type OscClient = { port :: Int }

newOscClient :: OscConfig -> Effect OscClient
newOscClient config = pure { port: config.port }

newOscEcho :: OscConfig -> Effect Scene
newOscEcho config = do
  client <- newOscClient config
  pure
    { mixer: initMixer client
    , sampler: initSampler client
    }

initMixer :: OscClient -> Mixer
initMixer _client =
  { setMasterVolume: logValue "Master volume"
  , setChannelVolume: \chan vol -> logValue "Channel volume" (chan /\ vol)
  }

initSampler :: OscClient -> Sampler
initSampler _client =
  { setTrack: logValue "Set track"
  , shiftTrack: logValue "Shift track"
  , shiftPart: logValue "Shift part"
  }

logValue :: forall a . Show a => String -> a -> Effect Unit
logValue title value = log $ title <> ": " <> show value
