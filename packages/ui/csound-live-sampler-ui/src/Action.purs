-- | Common interfaces
module Action
  ( ChannelId
  , TrackId
  , Scene
  , Mixer
  , Sampler
  , Info
  , FxId
  , FxParamId
  , SendId
  , SetMixer
  , SetChannel
  , SetMaster
  , SetFxParam
  , SetSendFx
  , SetAudioCard
  , SetFxBypass
  ) where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe)

type ChannelId = Int

type TrackId = Int

type Scene =
  { mixer :: Mixer
  , sampler :: Sampler
  , info :: Info
  }

type FxParamId =
  { channel :: Maybe ChannelId
  , name :: String
  , param :: String
  }

type FxId =
  { channel :: Maybe ChannelId
  , name :: String
  }

type SendId =
  { from :: ChannelId
  , to :: ChannelId
  }

type Mixer =
  { setMasterVolume :: Number -> Effect Unit
  , setChannelVolume :: ChannelId -> Number -> Effect Unit
  , setFxParam :: FxParamId -> Number -> Effect Unit
  , toggleFxBypass :: FxId -> Effect Unit
  , setSendFx :: SendId -> Number -> Effect Unit
  }

type Sampler =
  { setTrack :: TrackId -> Effect Unit
  , shiftTrack :: Int -> Effect Unit
  , shiftPart :: Int -> Effect Unit
  }

type Info =
  { getCurrentPart :: Effect Unit
  }

type SetMixer =
  { setMaster :: SetMaster
  , setChannel :: Int -> SetChannel
  , setFxParam :: FxParamId -> SetFxParam
  , setFxBypass :: FxId -> SetFxBypass
  , setSendFx :: SendId -> SetSendFx
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

type SetFxParam = Number -> Effect Unit

type SetFxBypass = Boolean -> Effect Unit

type SetSendFx = Number -> Effect Unit

type SetAudioCard =
  { setGain :: Int -> Number -> Effect Unit
  }


