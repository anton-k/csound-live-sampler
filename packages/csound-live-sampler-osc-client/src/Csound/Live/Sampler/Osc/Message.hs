module Csound.Live.Sampler.Osc.Message (
  ChannelId,
  TrackId,
  AudioInputId,
  FxName,
  ParamName,
  setMasterVolumeMessage,
  setChannelVolumeMessage,
  setTrackMessage,
  shiftPartMessage,
  shiftTrackMessage,
  nextPartMessage,
  prevPartMessage,
  nextTrackMessage,
  prevTrackMessage,
  setFxParamMessage,
  toggleMuteMessage,
  setChannelSendMessage,
  setAudioInputGainMessage,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.String
import Vivid.OSC

setMasterVolumeMessage :: Double -> OSC
setMasterVolumeMessage volume =
  OSC "/master/volume" [OSC_D volume]

-- | Starts from 1
type ChannelId = Int

-- | Starts from 1
type AudioInputId = Int

-- | Starts from 1
type TrackId = Int

type FxName = ByteString

type ParamName = ByteString

setChannelVolumeMessage :: ChannelId -> Double -> OSC
setChannelVolumeMessage channelId volume =
  OSC (toChannelAddr channelId "volume") [OSC_D volume]

setChannelSendMessage :: ChannelId -> ChannelId -> Double -> OSC
setChannelSendMessage channelFromId channelToId gain =
  OSC (toChannelAddr channelFromId $ "send/" <> fromString (show channelToId)) [OSC_D gain]

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

nextTrackMessage :: OSC
nextTrackMessage = shiftTrackMessage 1

prevTrackMessage :: OSC
prevTrackMessage = shiftTrackMessage (-1)

shiftTrackMessage :: Int -> OSC
shiftTrackMessage steps =
  OSC "/shiftTrack" [OSC_I $ fromIntegral steps]

setFxParamMessage :: FxName -> ParamName -> Double -> OSC
setFxParamMessage unit param value =
  OSC (path ["fx", unit, param]) [OSC_D value]

toggleMuteMessage :: ChannelId -> OSC
toggleMuteMessage channelId =
  OSC (toChannelAddr channelId "mute/toggle") []

setAudioInputGainMessage :: AudioInputId -> Double -> OSC
setAudioInputGainMessage inputId gain =
  OSC (path ["audio", "input", fromString (show inputId)]) [OSC_D gain]

path :: [ByteString] -> ByteString
path as = "/" <> ByteString.intercalate "/" as
