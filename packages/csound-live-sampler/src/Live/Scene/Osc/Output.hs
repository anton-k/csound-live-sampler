module Live.Scene.Osc.Output (
  setupOscOutput,
  send,
  sendCurrentSamplerPart,
) where

import Csound.Core
import Data.Maybe
import Data.Text qualified as Text
import Live.Scene.Common (ChannelId (..))
import Live.Scene.Mixer
import Live.Scene.Osc.Config
import Live.Scene.Sampler
import Live.Scene.Sampler.Engine (toAbsTimeRate)
import Live.Scene.Types

setupOscOutput :: Scene -> OscOutputConfig ChannelId -> SE ()
setupOscOutput scene config = do
  instr <- newProc $ \() -> do
    sendSampler scene.sampler config
    -- isTick <- scene.sampler.readTicks
    bpm <- scene.sampler.readBpm
    let
      isTick = metro (toAbsTimeRate bpm 0.25)
    mapM_ (sendChannelInfo config isTick scene.mixer) (fromMaybe [] config.channels)
  play instr [Note 0 (-1) ()]

sendSampler :: Sampler -> OscOutputConfig ChannelId -> SE ()
sendSampler sampler config = do
  sendTicks sampler config
  sendTrackChange sampler config

sendTicks :: Sampler -> OscOutputConfig ChannelId -> SE ()
sendTicks sampler config = do
  currentBeat <- sampler.currentBeat
  ticks <- sampler.readTicks
  send config ticks "/bpm/beats" currentBeat

sendTrackChange :: Sampler -> OscOutputConfig ChannelId -> SE ()
sendTrackChange sampler config = do
  isClipChange <- sampler.readIsMainClipChange
  sendCurrentSamplerPart config sampler isClipChange

sendCurrentSamplerPart :: OscOutputConfig ChannelId -> Sampler -> Sig -> SE ()
sendCurrentSamplerPart config sampler isClipChange = do
  clip <- sampler.readClip
  send config isClipChange "/part/change" clip

sendChannelInfo :: OscOutputConfig ChannelId -> Sig -> Mixer -> ChannelId -> SE ()
sendChannelInfo config isTick mixer channelId = do
  sendVolumeEnvelope config isTick mixer channelId
  sendVolumeChange config isTick mixer channelId
  sendVolumeMute config isTick mixer channelId

sendVolumeEnvelope :: OscOutputConfig ChannelId -> Sig -> Mixer -> ChannelId -> SE ()
sendVolumeEnvelope config isTick mixer channelId = do
  asig <- mixer.readChannel channelId
  let
    env = 5 * follow2 (toMono asig) 0.1 0.5
  send config isTick (toChannelAddr channelId "volume/envelope") env

sendVolumeChange :: OscOutputConfig ChannelId -> Sig -> Mixer -> ChannelId -> SE ()
sendVolumeChange config isTick mixer channelId = do
  vol <- mixer.readChannelVolume channelId
  let
    isChange = changed [vol]
  send config isChange (toChannelAddr channelId "volume/change") vol

sendVolumeMute :: OscOutputConfig ChannelId -> Sig -> Mixer -> ChannelId -> SE ()
sendVolumeMute config isTick mixer channelId = do
  mute <- mixer.readChannelMute channelId
  let
    isChange = changed [mute]
  send config isChange (toChannelAddr channelId "mute/change") (1 - mute)

toChannelAddr :: ChannelId -> String -> Str
toChannelAddr (ChannelId n) path =
  fromString $ "/channel/" <> show (n + 1) <> "/" <> path

--  when1 (ticks `equals` 1) $
--    printks "Beat: %d\n" 0 currentBeat

send :: (Tuple a) => OscOutputConfig ChannelId -> Sig -> Str -> a -> SE ()
send config kwhen dest values =
  oscSend kwhen (fromString $ Text.unpack config.address) (int config.port) dest values
