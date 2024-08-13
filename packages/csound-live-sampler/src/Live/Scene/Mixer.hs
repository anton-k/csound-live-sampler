{-| Mixer reads stereo audio from set of channels
and mixes them into single stereo audio output.

We can

* modify master volume
* modify individual channels volume
* toggle mute channels
* write audio to channels
* read output audio of the master bus

Mixer is an audio generator with inputs as channels and single output
-}
module Live.Scene.Mixer (
  Mixer (..),
  MixerConfig (..),
  MasterConfig (..),
  ChannelConfig (..),
  newMixer,
  FxParamId (..),
  MixerChannels,
  newMixerChannels,
  appendMixerChannels,
  readMixerMaster,
  readMixerChannels,
  module X,
) where

import Csound.Core
import Data.Boolean
import Data.Default
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe
import Live.Scene.Common (ChannelId (..), SendId (..), smoothControl)
import Live.Scene.Mixer.Config as X
import Live.Scene.Mixer.Fx (Bpm (..))
import Live.Scene.Mixer.Route (
  FxParamId (..),
  MixerRoute (..),
  MixerRouteFx (..),
  RouteDeps (..),
  toMixerRoute,
 )
import Safe (atMay)

data Mixer = Mixer
  -- audio
  { readMaster :: SE Sig2
  , readChannel :: ChannelId -> SE Sig2
  , -- control
    readMasterVolume :: SE Sig
  , toggleChannelMute :: ChannelId -> SE ()
  , readChannelMute :: ChannelId -> SE Sig
  , modifyChannelVolume :: ChannelId -> (Sig -> Sig) -> SE ()
  , readChannelVolume :: ChannelId -> SE Sig
  , modifyChannelSend :: ChannelId -> ChannelId -> (Sig -> Sig) -> SE ()
  , modifyMasterVolume :: (Sig -> Sig) -> SE ()
  , modifyFxParam :: FxParamId -> (Sig -> Sig) -> SE ()
  , setChannelVolume :: ChannelId -> Sig -> SE ()
  , setChannelSend :: SendId -> Sig -> SE ()
  , readChannelSend :: SendId -> SE Sig
  , setMasterVolume :: Sig -> SE ()
  , setFxParam :: FxParamId -> Sig -> SE ()
  , readFxParam :: FxParamId -> SE Sig
  , clean :: SE ()
  , config :: MixerConfig ChannelId
  }

data MixerChannels = MixerChannels
  { channels :: [Channel]
  , master :: Master
  }

newMixerChannels :: MixerConfig ChannelId -> SE MixerChannels
newMixerChannels config =
  MixerChannels <$> loadChannels config <*> loadMaster config

appendMixerChannels :: MixerChannels -> ChannelId -> Sig2 -> SE ()
appendMixerChannels (MixerChannels channels _master) (ChannelId channelId) ins =
  mapM_ (\chan -> modifyRef chan.audio (+ ins)) (channels `atMay` channelId)

readMixerMaster :: MixerChannels -> SE Sig2
readMixerMaster (MixerChannels _channels master) =
  readRef master.audio

readMixerChannels :: MixerChannels -> ChannelId -> SE Sig2
readMixerChannels (MixerChannels channels _master) (ChannelId channelId) =
  case channels `atMay` channelId of
    Just channel -> readRef channel.audio
    Nothing -> pure 0

newMixer :: MixerConfig ChannelId -> MixerChannels -> SE Sig -> SE Mixer
newMixer config channels readBpm = do
  st <- initSt channels
  let
    routeDeps = initRouteDeps st
  route <- toMixerRoute routeDeps (Bpm readBpm) config
  instrIds <- route.setupInstr
  let
    fxControls = route.fxControls instrIds
  route.launchInstr instrIds
  pure $
    Mixer
      { readMaster = readMixSt st
      , readMasterVolume = readMasterVolumeSt st
      , readChannel = readChannelSt st
      , modifyChannelVolume = modifyChannelVolumeSt st
      , readChannelVolume = readChannelVolumeSt st
      , modifyChannelSend = modifyChannelSendSt st
      , modifyMasterVolume = modifyMasterVolumeSt st
      , modifyFxParam = fxControls.modifyFxParam
      , setChannelVolume = setChannelVolumeSt st
      , setChannelSend = setChannelSendSt st
      , readChannelSend = readChannelSendSt st
      , setMasterVolume = setMasterVolumeSt st
      , setFxParam = fxControls.setFxParam
      , readFxParam = fxControls.readFxParam
      , toggleChannelMute = toggleChannelMuteSt st
      , readChannelMute = readChannelMuteSt st
      , clean = cleanSt st
      , config = config
      }

-- * mixer internal state

data St = St
  { master :: Master
  , channels :: [Channel]
  }

data Master = Master
  { volume :: Ref Sig
  , audio :: Ref Sig2
  , gain :: Maybe Float
  }

data Channel = Channel
  { volume :: Ref Sig
  , mute :: Ref Sig
  , gain :: Maybe Float
  , audio :: Ref Sig2
  , sendGains :: SendMap
  }

newtype SendMap = SendMap (IntMap (Ref Sig))

lookupSend :: ChannelId -> SendMap -> Maybe (Ref Sig)
lookupSend (ChannelId channelId) (SendMap refs) =
  IntMap.lookup channelId refs

initSt :: MixerChannels -> SE St
initSt MixerChannels{channels, master} =
  pure St{channels, master}

loadMaster :: MixerConfig ChannelId -> SE Master
loadMaster config =
  Master
    <$> newCtrlRef (float masterConfig.volume)
    <*> newRef 0
    <*> pure masterConfig.gain
  where
    masterConfig = fromMaybe def config.master

loadChannels :: MixerConfig ChannelId -> SE [Channel]
loadChannels config =
  mapM initChannel config.channels
  where
    initChannel channelConfig = do
      volume <- newCtrlRef (float channelConfig.volume)
      mute <- newCtrlRef 1
      let
        gain = channelConfig.gain
      audio <- newRef 0
      sendGains <- initSends channelConfig
      pure Channel{..}

    initSends :: ChannelConfig ChannelId -> SE SendMap
    initSends channelConfig =
      case channelConfig.sends of
        Just sends -> SendMap . IntMap.fromList <$> mapM toSendMapItem sends
        Nothing -> pure $ SendMap mempty

    toSendMapItem :: SendConfig ChannelId -> SE (Int, Ref Sig)
    toSendMapItem send =
      fmap (send.channel.unChannelId,) $ newCtrlRef (float send.gain)

readMixSt :: St -> SE Sig2
readMixSt St{..} = do
  volume <- readRef master.volume
  audio <- readRef master.audio
  pure $ withGain master.gain $ mul (smoothControl volume) audio

readMasterVolumeSt :: St -> SE Sig
readMasterVolumeSt st =
  readRef st.master.volume

readChannelSt :: St -> ChannelId -> SE Sig2
readChannelSt st channelId = do
  withChannelDef st channelId 0 $ \channel -> do
    volume <- readRef channel.volume
    audio <- readRef channel.audio
    pure $ withGain channel.gain $ mul (smoothControl volume) audio

writeChannelSt :: St -> ChannelId -> Sig2 -> SE ()
writeChannelSt st channelId audio =
  withChannel st channelId $ \channel -> do
    writeRef channel.audio audio

getChannel :: St -> ChannelId -> Maybe Channel
getChannel st (ChannelId n) = st.channels `atMay` n

withChannel :: St -> ChannelId -> (Channel -> SE ()) -> SE ()
withChannel st channelId f =
  mapM_ f (getChannel st channelId)

withChannelDef :: St -> ChannelId -> a -> (Channel -> SE a) -> SE a
withChannelDef st channelId defValue f =
  maybe (pure defValue) f (getChannel st channelId)

toggleChannelMuteSt :: St -> ChannelId -> SE ()
toggleChannelMuteSt st channelId =
  withChannel st channelId $ \channel ->
    modifyRef channel.mute $ \mute -> ifB (mute ==* 1) 0 1

modifyChannelVolumeSt :: St -> ChannelId -> (Sig -> Sig) -> SE ()
modifyChannelVolumeSt st channelId f =
  withChannel st channelId $ \channel ->
    modifyRef channel.volume f

readChannelVolumeSt :: St -> ChannelId -> SE Sig
readChannelVolumeSt st channelId =
  withChannelDef st channelId 0 $ \channel -> readRef channel.volume

readChannelMuteSt :: St -> ChannelId -> SE Sig
readChannelMuteSt st channelId =
  withChannelDef st channelId 0 $ \channel -> readRef channel.mute

setChannelVolumeSt :: St -> ChannelId -> Sig -> SE ()
setChannelVolumeSt st channelId ins =
  withChannel st channelId $ \channel ->
    writeRef channel.volume ins

modifyChannelSendSt :: St -> ChannelId -> ChannelId -> (Sig -> Sig) -> SE ()
modifyChannelSendSt st fromChannelId toChannelId f =
  withChannel st fromChannelId $ \channel ->
    mapM_ (\ref -> modifyRef ref f) (lookupSend toChannelId channel.sendGains)

setChannelSendSt :: St -> SendId -> Sig -> SE ()
setChannelSendSt st sendId ins =
  withChannel st sendId.from $ \channel ->
    mapM_ (\ref -> writeRef ref ins) (lookupSend sendId.to channel.sendGains)

readChannelSendSt :: St -> SendId -> SE Sig
readChannelSendSt st sendId =
  withChannelDef st sendId.from 0 $ \channel ->
    maybe (pure 0) readRef (lookupSend sendId.to channel.sendGains)

modifyMasterVolumeSt :: St -> (Sig -> Sig) -> SE ()
modifyMasterVolumeSt st f =
  modifyRef st.master.volume f

setMasterVolumeSt :: St -> Sig -> SE ()
setMasterVolumeSt st ins =
  writeRef st.master.volume ins

withGain :: (SigSpace a) => Maybe Float -> a -> a
withGain mValue audio =
  case mValue of
    Nothing -> audio
    Just value -> mul (float value) audio

cleanSt :: St -> SE ()
cleanSt st = do
  mapM_ (\channel -> writeRef channel.audio 0) st.channels
  writeRef st.master.audio 0

-------------------------------------------------------------------------------------
-- Routes

channelVolume :: Channel -> SE Sig
channelVolume channel = do
  volume <- readRef channel.volume
  mute <- readRef channel.mute
  pure $ withGain channel.gain (smoothControl $ volume * mute)

initRouteDeps :: St -> RouteDeps
initRouteDeps st =
  RouteDeps
    { readChannel = \n -> withChannelDef st n 0 (readRef . (.audio))
    , readChannelVolume = \n -> withChannelDef st n 1 channelVolume
    , readChannelSendGain = readChannelSendGainSt
    , writeChannel = \n -> writeChannelSt st n
    , appendChannel = \n -> appendChannelSt n
    , readMaster = readMasterSt
    , writeMaster = writeMasterSt
    , appendMaster = appendMasterSt
    }
  where
    appendChannelSt channel input = do
      val <- withChannelDef st channel 0 (readRef . (.audio))
      writeChannelSt st channel (val + input)

    appendMasterSt input = do
      val <- readRef st.master.audio
      writeRef st.master.audio (val + input)

    readMasterSt = readMixSt st

    writeMasterSt input =
      writeRef st.master.audio input

    readChannelSendGainSt :: ChannelId -> ChannelId -> SE Sig
    readChannelSendGainSt channelId sendId =
      withChannelDef st channelId 1 $ \channel ->
        case lookupSend sendId channel.sendGains of
          Just sendRef -> readRef sendRef
          Nothing -> pure 0
