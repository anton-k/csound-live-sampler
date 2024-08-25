-- | OSC messages for csound-live-sampler
module Osc.Message
  ( setMasterVolume
  , setChannelVolume
  , setTrack
  , nextTrack
  , prevTrack
  , nextPart
  , prevPart
  , shiftTrack
  , shiftPart
  , getCurrentPart
  , setFxParam
  , toggleFxBypass
  , setSendFx
  ) where

import Prelude
import Network.Osc (Osc, OscValue (..))
import Action (ChannelId, TrackId, FxId, FxParamId, SendId)
import Data.Maybe (Maybe, maybe)
import Data.String as String

setMasterVolume :: Number -> Osc
setMasterVolume volume =
  { address: "/master/volume"
  , args: [OscDouble volume]
  }

setChannelVolume :: ChannelId -> Number -> Osc
setChannelVolume channelId volume =
  { address: "/channel/" <> show channelId <> "/volume"
  , args: [OscDouble volume]
  }

setTrack :: TrackId -> Osc
setTrack trackId =
  { address: "/track"
  , args: [OscInt trackId]
  }

shiftTrack :: Int -> Osc
shiftTrack n =
  { address: "/shiftTrack"
  , args: [OscInt n]
  }

shiftPart :: Int -> Osc
shiftPart n =
  { address: "/shiftPart"
  , args: [OscInt n]
  }

nextTrack :: Osc
nextTrack = shiftTrack 1

prevTrack :: Osc
prevTrack = shiftTrack (-1)

nextPart :: Osc
nextPart = shiftPart 1

prevPart :: Osc
prevPart = shiftPart (-1)

getCurrentPart :: Osc
getCurrentPart =
  { address: "/getCurrentPart"
  , args: []
  }

toFxPrefix :: Maybe ChannelId -> String
toFxPrefix chan =
  "/" <> maybe "master" (\n -> "channel/" <> show n) chan

setFxParam :: FxParamId -> Number -> Osc
setFxParam paramId value =
  { address: String.joinWith "/" [channelAddr, "fx", "param", paramId.name, paramId.param]
  , args: [OscDouble value]
  }
  where
    channelAddr = toFxPrefix paramId.channel

toggleFxBypass :: FxId -> Osc
toggleFxBypass fxId =
  { address: String.joinWith "/" [channelAddr, "fx", "bypass", "toggle", fxId.name]
  , args: []
  }
  where
    channelAddr = toFxPrefix fxId.channel

setSendFx :: SendId -> Number -> Osc
setSendFx sendId value =
  { address: String.joinWith "/" ["/channel", show sendId.from, "send", show sendId.to]
  , args: [OscDouble value]
  }
