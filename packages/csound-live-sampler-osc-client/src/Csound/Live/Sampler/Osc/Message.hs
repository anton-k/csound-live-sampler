module Csound.Live.Sampler.Osc.Message (
  ChannelId,
  TrackId,
  setMasterVolumeMessage,
  setChannelVolumeMessage,
  setTrackMessage,
  shiftPartMessage,
  shiftTrackMessage,
  nextPartMessage,
  prevPartMessage,
) where

import Data.ByteString (ByteString)
import Data.String
import Vivid.OSC

setMasterVolumeMessage :: Double -> OSC
setMasterVolumeMessage volume =
  OSC "/master/volume" [OSC_D volume]

-- | Starts from 1
type ChannelId = Int

-- | Starts from 1
type TrackId = Int

setChannelVolumeMessage :: ChannelId -> Double -> OSC
setChannelVolumeMessage channelId volume =
  OSC (toChannelAddr channelId "volume") [OSC_D volume]

toChannelAddr :: ChannelId -> ByteString -> ByteString
toChannelAddr channelId addr =
  "/channel/" <> fromString (show channelId) <> "/" <> addr

setTrackMessage :: TrackId -> OSC
setTrackMessage trackId =
  OSC "/track" [OSC_I $ fromIntegral trackId]

shiftPartMessage :: Int -> OSC
shiftPartMessage steps =
  OSC "/shiftPart" [OSC_I $ fromIntegral steps]

nextPartMessage :: OSC
nextPartMessage = shiftPartMessage 1

prevPartMessage :: OSC
prevPartMessage = shiftPartMessage (-1)

shiftTrackMessage :: Int -> OSC
shiftTrackMessage steps =
  OSC "/shiftTrack" [OSC_I $ fromIntegral steps]
