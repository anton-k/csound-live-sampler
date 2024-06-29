-- | OSC messages for csound-live-sampler
module Osc.Message
  ( setMasterVolumeMessage
  , setChannelVolumeMessage
  , setTrack
  , nextTrack
  , prevTrack
  , nextPart
  , prevPart
  , shiftTrack
  , shiftPart
  ) where

import Prelude
import Network.Osc (Osc, OscValue (..))
import Action (ChannelId, TrackId)

setMasterVolumeMessage :: Number -> Osc
setMasterVolumeMessage volume =
  { address: "/master/volume"
  , args: [OscFloat volume]
  }

setChannelVolumeMessage :: ChannelId -> Number -> Osc
setChannelVolumeMessage channelId volume =
  { address: "/channell/" <> show channelId <> "/volume"
  , args: [OscFloat volume]
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
