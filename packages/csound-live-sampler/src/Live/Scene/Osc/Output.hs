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
import Live.Scene.AudioCard
import Live.Scene.AudioCard.Config
import Live.Scene.Common (AudioInputId (..), ChannelId (..), SendId (..))
import Live.Scene.Mixer
import Live.Scene.Mixer.Fx (fxUnitName, toFxParamNameInitMap)
import Live.Scene.Mixer.Fx.Config
import Live.Scene.Osc.Config
import Live.Scene.Osc.Input (OscConfigs (..))
import Live.Scene.Sampler
import Live.Scene.Sampler.Engine (toAbsTimeRate)
import Live.Scene.Types

type FxParamMap = Map (Maybe ChannelId) [FxParamId]
type SendMap = Map ChannelId [SendId]

setupOscOutput :: Scene -> OscConfigs -> OscOutputConfig ChannelId -> SE ()
setupOscOutput scene oscConfig config = do
  instr <- newProc $ \() -> do
    sendSampler scene.sampler config
    bpm <- scene.sampler.readBpm
    let
      isTick = metro (toAbsTimeRate bpm 0.25)
    sendMasterInfo config isTick scene.mixer fxParams
    mapM_ (sendChannelInfo config isTick scene.mixer fxParams sendParams) (fromMaybe [] config.channels)
    sendAudioCard config oscConfig.card scene.audio isTick
  play instr [Note 0 (-1) ()]
  where
    fxParams = initFxParams scene.mixer
    sendParams = initSendParams scene.mixer

initFxParams :: Mixer -> FxParamMap
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

initSendParams :: Mixer -> SendMap
initSendParams mixer =
  Map.fromList $
    filter (not . null . snd) $
      catMaybes $
        zipWith channelSend [0 ..] mixer.config.channels
  where
    channelSend n config =
      fmap
        ( \sends ->
            let
              from = ChannelId n
             in
              (from, fmap (\sendConfig -> SendId{from, to = sendConfig.channel}) sends)
        )
        config.sends

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

sendMasterInfo ::
  OscOutputConfig ChannelId ->
  Sig ->
  Mixer ->
  FxParamMap ->
  SE ()
sendMasterInfo config isTick mixer fxParams = do
  sendMasterVolumeEnvelope config isTick mixer
  sendMasterVolumeChange config mixer
  mapM_ (mapM_ (sendFxParamChange config mixer)) (Map.lookup Nothing fxParams)

sendMasterVolumeEnvelope :: OscOutputConfig ChannelId -> Sig -> Mixer -> SE ()
sendMasterVolumeEnvelope config isTick mixer = do
  asig <- mixer.readMaster
  let
    env = 4.5 * follow2 (toMono asig) 0.1 0.5
  send config isTick (toMasterAddr "volume/envelope") env

sendMasterVolumeChange :: OscOutputConfig ChannelId -> Mixer -> SE ()
sendMasterVolumeChange config mixer =
  changeChannelBy config Nothing "volume" "" mixer.readMasterVolume

sendChannelInfo ::
  OscOutputConfig ChannelId ->
  Sig ->
  Mixer ->
  FxParamMap ->
  SendMap ->
  ChannelId ->
  SE ()
sendChannelInfo config isTick mixer fxParams sendParams channelId = do
  sendVolumeEnvelope config isTick mixer channelId
  sendVolumeChange config mixer channelId
  sendVolumeMute config mixer channelId
  mapM_ (mapM_ (sendSendChange config mixer)) (Map.lookup channelId sendParams)
  mapM_ (mapM_ (sendFxParamChange config mixer)) (Map.lookup (Just channelId) fxParams)

sendVolumeEnvelope :: OscOutputConfig ChannelId -> Sig -> Mixer -> ChannelId -> SE ()
sendVolumeEnvelope config isTick mixer channelId = do
  asig <- mixer.readChannel channelId
  let
    env = 5 * follow2 (toMono asig) 0.1 0.5
  send config isTick (toChannelAddr channelId "volume/envelope") env

sendVolumeChange :: OscOutputConfig ChannelId -> Mixer -> ChannelId -> SE ()
sendVolumeChange config mixer channelId =
  changeChannelBy config (Just channelId) "volume" "" (mixer.readChannelVolume channelId)

sendVolumeMute :: OscOutputConfig ChannelId -> Mixer -> ChannelId -> SE ()
sendVolumeMute config mixer channelId = do
  changeChannelBy config (Just channelId) "mute" "" (invertMute <$> mixer.readChannelMute channelId)
  where
    invertMute x = 1 - x

sendFxParamChange :: OscOutputConfig ChannelId -> Mixer -> FxParamId -> SE ()
sendFxParamChange config mixer paramId = do
  changeChannelBy config paramId.channel "fx" paramAddr (mixer.readFxParam paramId)
  where
    paramAddr = Text.unpack $ paramId.name <> "/" <> paramId.param

sendSendChange :: OscOutputConfig ChannelId -> Mixer -> SendId -> SE ()
sendSendChange config mixer sendId =
  changeChannelBy config (Just sendId.from) "send" (show sendId.to) (mixer.readChannelSend sendId)

changeChannelBy :: OscOutputConfig ChannelId -> Maybe ChannelId -> String -> String -> SE Sig -> SE ()
changeChannelBy config mChannelId tag addr readParam = do
  param <- readParam
  let
    isChange = changed [param]
  send
    config
    isChange
    ( maybe toMasterAddr toChannelAddr mChannelId $
        tag <> (if null addr then "/change" else "/change/" <> addr)
    )
    param

toMasterAddr :: String -> Str
toMasterAddr path =
  fromString $ "/master/" <> path

toChannelAddr :: ChannelId -> String -> Str
toChannelAddr (ChannelId n) path =
  fromString $ "/channel/" <> show (n + 1) <> "/" <> path

--  when1 (ticks `equals` 1) $
--    printks "Beat: %d\n" 0 currentBeat

sendAudioCard :: OscOutputConfig ChannelId -> AudioConfig ChannelId -> AudioCard -> Sig -> SE ()
sendAudioCard config cardConfig audio isTick =
  mapM_ (mapM_ (sendAudioCardInput config audio isTick)) cardConfig.inputs

sendAudioCardInput :: OscOutputConfig ChannelId -> AudioCard -> Sig -> AudioInputConfig ChannelId -> SE ()
sendAudioCardInput config audio isTick input = do
  gain <- audio.readInputGain inputId
  send config isTick (fromString $ "/audio/input/" <> name <> "/gain/change/") gain
  where
    name = show (chan + 1)
    inputId = AudioInputId chan
    ChannelId chan = getChannelIdConfig input

send :: (Tuple a) => OscOutputConfig ChannelId -> Sig -> Str -> a -> SE ()
send config kwhen dest values =
  oscSend kwhen (fromString $ Text.unpack config.address) (int config.port) dest values
