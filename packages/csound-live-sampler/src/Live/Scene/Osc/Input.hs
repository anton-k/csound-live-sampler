module Live.Scene.Osc.Input (
  setupOscInput,
  OscConfigs (..),
  OscInputDep (..),
) where

import Control.Monad
import Csound.Core
import Data.Default
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Live.Scene.AudioCard
import Live.Scene.AudioCard.Config
import Live.Scene.Common (AudioInputId (..), ChannelId (..), SendId (..))
import Live.Scene.Mixer
import Live.Scene.Mixer.Fx (FxId (..), toFxParamNameInitMap)
import Live.Scene.Mixer.Fx.Config
import Live.Scene.Mixer.Fx.Unit
import Live.Scene.Osc.Config
import Live.Scene.Sampler
import Live.Scene.Sampler.Playlist
import Live.Scene.Types

data OscConfigs = OscConfigs
  { osc :: OscConfig ChannelId
  , mixer :: MixerConfig ChannelId
  , sampler :: SamplerConfig ChannelId
  , card :: AudioConfig ChannelId
  }

data OscInputDep = OscInputDep
  { sendUiInfo :: BoolSig -> Str -> SE ()
  , sendCurrentPart :: BoolSig -> SE ()
  }

setupOscInput :: Scene -> OscConfigs -> OscInputDep -> OscInputConfig -> SE ()
setupOscInput scene oscConfig dep config = do
  instrRef <- newProc (\() -> oscInputInstr scene oscConfig dep config)
  play instrRef [Note 0 (-1) ()]

oscInputInstr :: Scene -> OscConfigs -> OscInputDep -> OscInputConfig -> SE ()
oscInputInstr scene oscConfig dep config = do
  listenMixer scene.mixer oscConfig.mixer oscHandle
  listenSampler scene.sampler oscHandle
  listenAudioCard scene.audio oscConfig.card oscHandle
  listenUiInfo oscHandle dep
  listenGetCurrentPart oscHandle dep
  where
    oscHandle = oscInit (int config.port)

listenMixer :: Mixer -> MixerConfig ChannelId -> OscHandle -> SE ()
listenMixer mixer mixerConfig oscHandle = do
  listenMaster mixer (fromMaybe def mixerConfig.master) oscHandle
  zipWithM_ (listenChannel mixer oscHandle) [1 ..] mixerConfig.channels

listenMaster :: Mixer -> MasterConfig -> OscHandle -> SE ()
listenMaster mixer config oscHandle = do
  listenMasterVolume mixer config oscHandle
  mapM_ (\unit -> listenFxUnit unit mixer oscHandle Nothing) (fromMaybe [] $ config.fxs)

listenMasterVolume :: Mixer -> MasterConfig -> OscHandle -> SE ()
listenMasterVolume mixer config oscHandle = do
  listenFloat oscHandle "/master/volume" config.volume $ \volume ->
    mixer.setMasterVolume volume

listenChannel :: Mixer -> OscHandle -> Int -> ChannelConfig ChannelId -> SE ()
listenChannel mixer oscHandle oscChannelId config = do
  listenChannelVolume config mixer oscHandle oscChannelId
  listenChannelSend config mixer oscHandle oscChannelId
  mapM_ (\unit -> listenFxUnit unit mixer oscHandle (Just oscChannelId)) (fromMaybe [] $ config.fxs)
  listenChannelMute mixer oscHandle oscChannelId

listenChannelVolume :: ChannelConfig ChannelId -> Mixer -> OscHandle -> Int -> SE ()
listenChannelVolume config mixer oscHandle oscChannelId =
  listenFloat oscHandle (toChannelAddr oscChannelId "volume") config.volume $ \volume ->
    mixer.setChannelVolume (toChannelId oscChannelId) volume

listenChannelSend :: ChannelConfig ChannelId -> Mixer -> OscHandle -> Int -> SE ()
listenChannelSend config mixer oscHandle oscChannelId =
  mapM_ onSend (fromMaybe [] config.sends)
  where
    onSend :: SendConfig ChannelId -> SE ()
    onSend sendConfig =
      listenFloat oscHandle sendAddr sendConfig.gain $ \gain ->
        mixer.setChannelSend (SendId{from = toChannelId oscChannelId, to = sendConfig.channel}) gain
      where
        sendAddr = toChannelAddr oscChannelId $ "send/" <> show (succ $ unChannelId sendConfig.channel)

listenFxUnit :: FxUnit -> Mixer -> OscHandle -> Maybe Int -> SE ()
listenFxUnit unit mixer oscHandle mChannelId = do
  listenFxParams unit mixer oscHandle mChannelId
  listenFxBypass unit mixer oscHandle mChannelId

listenFxParams :: FxUnit -> Mixer -> OscHandle -> Maybe Int -> SE ()
listenFxParams unit mixer oscHandle mChannelId =
  mapM_ (uncurry $ onFxUnit unit.name) $ Map.toList $ toFxParamNameInitMap unit
  where
    onFxUnit :: Text -> FxParamName -> Float -> SE ()
    onFxUnit unitName paramName initValue =
      listenFloat oscHandle fxAddr initValue $ \val ->
        mixer.setFxParam (FxParamId (toChannelId <$> mChannelId) unitName paramName) val
      where
        fxAddr =
          maybe toMasterAddr toChannelAddr mChannelId $
            "fx/param/" <> Text.unpack (mconcat [unitName, "/", paramName])

listenFxBypass :: FxUnit -> Mixer -> OscHandle -> Maybe Int -> SE ()
listenFxBypass unit mixer oscHandle mChannelId =
  listenUnit oscHandle fxAddr $
    mixer.toggleFxBypass (FxId (toChannelId <$> mChannelId) unit.name)
  where
    fxAddr =
      maybe toMasterAddr toChannelAddr mChannelId $
        "fx/bypass/toggle/" <> Text.unpack unit.name

listenChannelMute :: Mixer -> OscHandle -> Int -> SE ()
listenChannelMute mixer oscHandle oscChannelId = do
  listenUnit oscHandle (toChannelAddr oscChannelId "mute/toggle") $
    mixer.toggleChannelMute (toChannelId oscChannelId)

toChannelId :: Int -> ChannelId
toChannelId oscChannelId = ChannelId (oscChannelId - 1)

toMasterAddr :: String -> String
toMasterAddr name = "/master/" <> name

toChannelAddr :: Int -> String -> String
toChannelAddr oscChannelId name = mconcat ["/channel/", show oscChannelId, "/", name]

listenSampler :: Sampler -> OscHandle -> SE ()
listenSampler sampler oscHandle = do
  listenSetTrack sampler oscHandle
  listenSetPart sampler oscHandle
  listenShiftTrack sampler oscHandle
  listenShiftPart sampler oscHandle

-- listenPlayClip sampler oscHandle

listenSetTrack :: Sampler -> OscHandle -> SE ()
listenSetTrack sampler oscHandle =
  listenFloat oscHandle "/track" 0 $ \trackId ->
    setTrack sampler.cursor (toTrackId trackId)

toTrackId :: Sig -> TrackId
toTrackId trackId = TrackId $ toD (trackId - 1)

listenSetPart :: Sampler -> OscHandle -> SE ()
listenSetPart _sampler _oscHandle = pure () -- TODO
{-
  listenFloat oscHandle "/part" 0 $ \trackId ->
    setPart sampler.cursor (TrackId $ toD trackId)
-}

listenShiftTrack :: Sampler -> OscHandle -> SE ()
listenShiftTrack sampler oscHandle =
  listenFloat oscHandle "/shiftTrack" 0 $ \steps ->
    sampler.cursor.modifyTrack (+ steps)

listenShiftPart :: Sampler -> OscHandle -> SE ()
listenShiftPart sampler oscHandle =
  listenFloat oscHandle "/shiftPart" 0 $ \steps ->
    sampler.cursor.modifyPart (+ steps)

{- TODO
listenPlayClip :: Sampler -> OscHandle -> SE ()
listenPlayClip sampler oscHandle =
  listenFloat2 oscHandle "/playClip" (0, 0) $ \(columnId, clipId) ->
    sampler.playExtraClip (ColumnName columnId) (ClipName clipId)
-}

listenAudioCard :: AudioCard -> AudioConfig ChannelId -> OscHandle -> SE ()
listenAudioCard card cardConfig oscHandle =
  zipWithM_ onInput [1 ..] (fromMaybe [] cardConfig.inputs)
  where
    onInput :: Int -> AudioInputConfig ChannelId -> SE ()
    onInput index inputConfig =
      listenFloat oscHandle audioCardAddr (fromMaybe 1 $ getInputGainConfig inputConfig) $ \gain ->
        card.setInputGain audioInputId gain
      where
        audioCardAddr = "/audio/input/" <> show index <> "/gain"
        audioInputId = AudioInputId (index - 1)

listenFloat :: OscHandle -> String -> Float -> (Sig -> SE ()) -> SE ()
listenFloat oscHandle addr initValue cont = do
  ref <- newLocalCtrlRef (float initValue)
  hasMessage <- oscListen oscHandle (fromString addr) ref
  when1 hasMessage $ cont =<< readRef ref

{-
listenFloat2 :: OscHandle -> String -> (Float, Float) -> (Sig2 -> SE ()) -> SE ()
listenFloat2 oscHandle addr (initValue1, initValue2) cont = do
  ref <- newLocalCtrlRef (float initValue1, float initValue2)
  hasMessage <- oscListen oscHandle (fromString addr) ref
  when1 hasMessage $ cont =<< readRef ref
-}

listenUnit :: OscHandle -> String -> SE () -> SE ()
listenUnit oscHandle addr cont = do
  ref <- newLocalCtrlRef ()
  hasMessage <- oscListen oscHandle (fromString addr) ref
  when1 hasMessage cont

listenUiInfo :: OscHandle -> OscInputDep -> SE ()
listenUiInfo oscHandle dep = do
  ref <- newLocalCtrlRef ()
  hasMessage <- oscListen oscHandle "/ui/info/get" ref
  dep.sendUiInfo hasMessage "/ui/info/put"

listenGetCurrentPart :: OscHandle -> OscInputDep -> SE ()
listenGetCurrentPart oscHandle dep = do
  ref <- newLocalCtrlRef ()
  hasMessage <- oscListen oscHandle "/getCurrentPart" ref
  dep.sendCurrentPart hasMessage
