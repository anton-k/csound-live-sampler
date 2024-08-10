module Live.Scene.Osc.Output (
  setupOscOutput,
  send,
  sendCurrentSamplerPart,
) where

import Csound.Core
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text qualified as Text
import Live.Scene.Common (ChannelId (..))
import Live.Scene.Mixer
import Live.Scene.Mixer.Fx (fxUnitName, toFxParamNameInitMap)
import Live.Scene.Mixer.Fx.Config
import Live.Scene.Osc.Config
import Live.Scene.Sampler
import Live.Scene.Sampler.Engine (toAbsTimeRate)
import Live.Scene.Types

setupOscOutput :: Scene -> OscOutputConfig ChannelId -> SE ()
setupOscOutput scene config = do
  instr <- newProc $ \() -> do
    sendSampler scene.sampler config
    bpm <- scene.sampler.readBpm
    let
      isTick = metro (toAbsTimeRate bpm 0.25)
    mapM_ (sendChannelInfo config isTick scene.mixer fxParams) (fromMaybe [] config.channels)
  play instr [Note 0 (-1) ()]
  where
    fxParams = initFxParams scene.mixer

initFxParams :: Mixer -> Map (Maybe ChannelId) [FxParamId]
initFxParams mixer =
  Map.fromList $
    filter (not . null . snd) $
      catMaybes $
        maybe [] (pure . masterFx) mixer.config.master
          <> zipWith channelFx [0 ..] mixer.config.channels
  where
    masterFx config = fmap (\fxs -> (Nothing, toParamId Nothing =<< fxs)) config.fxs

    channelFx n config =
      fmap
        ( \fxs ->
            let
              chan = ChannelId n
             in
              (Just chan, toParamId (Just chan) =<< fxs)
        )
        config.fxs

    toParamId :: Maybe ChannelId -> FxUnit -> [FxParamId]
    toParamId mChannel unit =
      fmap (FxParamId mChannel name) (Map.keys $ toFxParamNameInitMap unit)
      where
        name = fxUnitName unit

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

sendMasterInfo :: OscOutputConfig ChannelId -> Sig -> Mixer -> Map (Maybe ChannelId) [FxParamId] -> SE ()
sendMasterInfo config isTick mixer fxParams = do
  sendMasterVolumeEnvelope config isTick mixer
  sendMasterVolumeChange config isTick mixer
  mapM_ (mapM_ (sendFxParamChange config isTick mixer)) (Map.lookup Nothing fxParams)

sendMasterVolumeEnvelope :: OscOutputConfig ChannelId -> Sig -> Mixer -> SE ()
sendMasterVolumeEnvelope config isTick mixer = do
  asig <- mixer.readMaster
  let
    env = 5 * follow2 (toMono asig) 0.1 0.5
  send config isTick (toMasterAddr "volume/envelope") env

sendMasterVolumeChange :: OscOutputConfig ChannelId -> Sig -> Mixer -> SE ()
sendMasterVolumeChange config isTick mixer = do
  vol <- mixer.readMasterVolume
  let
    isChange = changed [vol]
  send config isChange (toMasterAddr "volume/change") vol

sendChannelInfo :: OscOutputConfig ChannelId -> Sig -> Mixer -> Map (Maybe ChannelId) [FxParamId] -> ChannelId -> SE ()
sendChannelInfo config isTick mixer fxParams channelId = do
  sendVolumeEnvelope config isTick mixer channelId
  sendVolumeChange config isTick mixer channelId
  sendVolumeMute config isTick mixer channelId
  mapM_ (mapM_ (sendFxParamChange config isTick mixer)) (Map.lookup (Just channelId) fxParams)

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

sendFxParamChange :: OscOutputConfig ChannelId -> Sig -> Mixer -> FxParamId -> SE ()
sendFxParamChange config isTick mixer paramId = do
  param <- mixer.readFxParam paramId
  let
    isChange = changed [param]
  send
    config
    isChange
    ( maybe toMasterAddr toChannelAddr paramId.channel $
        "fx/change/" <> paramAddr
    )
    param
  where
    paramAddr = Text.unpack $ paramId.name <> "/" <> paramId.param

toMasterAddr :: String -> Str
toMasterAddr path =
  fromString $ "/master/" <> path

toChannelAddr :: ChannelId -> String -> Str
toChannelAddr (ChannelId n) path =
  fromString $ "/channel/" <> show (n + 1) <> "/" <> path

--  when1 (ticks `equals` 1) $
--    printks "Beat: %d\n" 0 currentBeat

send :: (Tuple a) => OscOutputConfig ChannelId -> Sig -> Str -> a -> SE ()
send config kwhen dest values =
  oscSend kwhen (fromString $ Text.unpack config.address) (int config.port) dest values
