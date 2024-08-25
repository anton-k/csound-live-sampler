module Live.Scene.Osc.Output (
  setupOscOutput,
  send,
  sendCurrentSamplerPart,
  sendAllControls,
) where

import Control.Monad
import Csound.Core
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text qualified as Text
import Live.Scene.AudioCard
import Live.Scene.AudioCard.Config
import Live.Scene.Common (AudioInputId (..), ChannelId (..), SendId (..))
import Live.Scene.Mixer
import Live.Scene.Mixer.Fx (FxId (..), toFxParamNameInitMap)
import Live.Scene.Mixer.Fx.Config
import Live.Scene.Osc.Config
import Live.Scene.Osc.Input (OscConfigs (..))
import Live.Scene.Sampler
import Live.Scene.Sampler.Engine (toAbsTimeRate)
import Live.Scene.Types

data SendType = Instant | OnChange
  deriving (Show, Eq)

whenChange :: SendType -> SE () -> SE ()
whenChange ty act =
  when (ty == OnChange) $ act

type FxParamMap = Map (Maybe ChannelId) FxInfo
type SendMap = Map ChannelId [SendId]

data FxInfo = FxInfo
  { params :: [FxParamId]
  , units :: [FxId]
  }

instance Semigroup FxInfo where
  (<>) infoA infoB = FxInfo (infoA.params <> infoB.params) (infoA.units <> infoB.units)

instance Monoid FxInfo where
  mempty = FxInfo mempty mempty

setupOscOutput :: Scene -> OscConfigs -> OscOutputConfig ChannelId -> SE ()
setupOscOutput scene oscConfig config = do
  instr <- newProc $ \() -> do
    sendSampler scene.sampler config
    bpm <- scene.sampler.readBpm
    let
      isTick = metro (toAbsTimeRate bpm 0.25)
    sendMasterInfo OnChange config isTick scene.mixer fxParams
    mapM_ (sendChannelInfo OnChange config isTick scene.mixer fxParams sendParams) (fromMaybe [] config.channels)
    sendAudioCard config oscConfig.card scene.audio isTick
  play instr [Note 0 (-1) ()]
  where
    fxParams = initFxParams scene.mixer
    sendParams = initSendParams scene.mixer

initFxParams :: Mixer -> FxParamMap
initFxParams mixer =
  Map.fromList $
    filter (not . isNullFxInfo . snd) $
      catMaybes $
        maybe [] (pure . masterFx) mixer.config.master
          <> zipWith channelFx [0 ..] mixer.config.channels
  where
    isNullFxInfo info = null info.params && null info.units

    masterFx :: MasterConfig -> Maybe (Maybe ChannelId, FxInfo)
    masterFx config =
      fmap
        ( \fxs ->
            ( Nothing
            , FxInfo
                { params = toParamId Nothing =<< fxs
                , units = (\fx -> FxId Nothing fx.name) <$> fxs
                }
            )
        )
        config.fxs

    channelFx :: Int -> ChannelConfig ChannelId -> Maybe (Maybe ChannelId, FxInfo)
    channelFx n config =
      fmap
        ( \fxs ->
            let
              chan = ChannelId n
             in
              ( Just chan
              , FxInfo
                  { params = toParamId (Just chan) =<< fxs
                  , units = (\fx -> FxId (Just chan) fx.name) <$> fxs
                  }
              )
        )
        config.fxs

    toParamId :: Maybe ChannelId -> FxUnit -> [FxParamId]
    toParamId mChannel unit =
      fmap (FxParamId mChannel unit.name) (Map.keys $ toFxParamNameInitMap unit)

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
  SendType ->
  OscOutputConfig ChannelId ->
  Sig ->
  Mixer ->
  FxParamMap ->
  SE ()
sendMasterInfo sendType config isTick mixer fxParams = do
  whenChange sendType $ sendMasterVolumeEnvelope config isTick mixer
  sendMasterVolumeChange mChange config mixer
  mapM_ (sendFxInfo mChange config mixer) (Map.lookup Nothing fxParams)
  where
    mChange = case sendType of
      OnChange -> Nothing
      Instant -> Just isTick

sendMasterVolumeEnvelope :: OscOutputConfig ChannelId -> Sig -> Mixer -> SE ()
sendMasterVolumeEnvelope config isTick mixer = do
  asig <- mixer.readMaster
  let
    env = 4.5 * follow2 (toMono asig) 0.1 0.5
  send config isTick (toMasterAddr "volume/envelope") env

sendMasterVolumeChange :: Maybe Sig -> OscOutputConfig ChannelId -> Mixer -> SE ()
sendMasterVolumeChange mChange config mixer =
  changeChannelBy mChange config Nothing "volume" "" mixer.readMasterVolume

sendChannelInfo ::
  SendType ->
  OscOutputConfig ChannelId ->
  Sig ->
  Mixer ->
  FxParamMap ->
  SendMap ->
  ChannelId ->
  SE ()
sendChannelInfo sendType config isTick mixer fxParams sendParams channelId = do
  whenChange sendType $ sendVolumeEnvelope config isTick mixer channelId
  sendVolumeChange mChange config mixer channelId
  sendVolumeMute mChange config mixer channelId
  mapM_ (mapM_ (sendSendChange mChange config mixer)) (Map.lookup channelId sendParams)
  mapM_ (sendFxInfo mChange config mixer) (Map.lookup (Just channelId) fxParams)
  where
    mChange = case sendType of
      OnChange -> Nothing
      Instant -> Just isTick

sendVolumeEnvelope :: OscOutputConfig ChannelId -> Sig -> Mixer -> ChannelId -> SE ()
sendVolumeEnvelope config isTick mixer channelId = do
  asig <- mixer.readChannel channelId
  let
    env = 5 * follow2 (toMono asig) 0.1 0.5
  send config isTick (toChannelAddr channelId "volume/envelope") env

sendVolumeChange :: Maybe Sig -> OscOutputConfig ChannelId -> Mixer -> ChannelId -> SE ()
sendVolumeChange mChange config mixer channelId =
  changeChannelBy mChange config (Just channelId) "volume" "" (mixer.readChannelVolume channelId)

sendVolumeMute :: Maybe Sig -> OscOutputConfig ChannelId -> Mixer -> ChannelId -> SE ()
sendVolumeMute mChange config mixer channelId = do
  changeChannelBy mChange config (Just channelId) "mute" "" (invertMute <$> mixer.readChannelMute channelId)
  where
    invertMute x = 1 - x

sendFxInfo :: Maybe Sig -> OscOutputConfig ChannelId -> Mixer -> FxInfo -> SE ()
sendFxInfo mChange config mixer info = do
  mapM_ (sendFxParamChange mChange config mixer) info.params
  mapM_ (sendFxBypassChange mChange config mixer) info.units

sendFxParamChange :: Maybe Sig -> OscOutputConfig ChannelId -> Mixer -> FxParamId -> SE ()
sendFxParamChange mChange config mixer paramId = do
  changeChannelBy mChange config paramId.channel "fx/param" paramAddr (mixer.readFxParam paramId)
  where
    paramAddr = Text.unpack $ paramId.name <> "/" <> paramId.param

sendFxBypassChange :: Maybe Sig -> OscOutputConfig ChannelId -> Mixer -> FxId -> SE ()
sendFxBypassChange mChange config mixer fxId =
  changeChannelBy mChange config fxId.channel "fx/bypass" bypassAddr (mixer.readFxBypass fxId)
  where
    bypassAddr = Text.unpack fxId.name

sendSendChange :: Maybe Sig -> OscOutputConfig ChannelId -> Mixer -> SendId -> SE ()
sendSendChange mChange config mixer sendId =
  changeChannelBy mChange config (Just sendId.from) "send" (show (sendId.to.unChannelId + 1)) (mixer.readChannelSend sendId)

changeChannelBy :: Maybe Sig -> OscOutputConfig ChannelId -> Maybe ChannelId -> String -> String -> SE Sig -> SE ()
changeChannelBy mChange config mChannelId tag addr readParam = do
  param <- readParam
  let
    isChange = fromMaybe (changed [param]) mChange
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

-- | We resend all controls after client asks for UI-info
sendAllControls :: Scene -> OscConfigs -> Sig -> OscOutputConfig ChannelId -> SE ()
sendAllControls scene _oscConfig isTick config = do
  sendMasterInfo Instant config isTick scene.mixer fxParams
  mapM_ (sendChannelInfo Instant config isTick scene.mixer fxParams sendParams) (fromMaybe [] config.channels)
  where
    fxParams = initFxParams scene.mixer
    sendParams = initSendParams scene.mixer
