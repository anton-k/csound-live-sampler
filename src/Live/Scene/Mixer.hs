-- | Mixer reads stereo audio from set of channels
-- and mixes them into single stereo audio output.
--
-- We can
--
-- * modify master volume
-- * modify individual channels volume
-- * toggle mute channels
-- * write audio to channels
-- * read output audio of the master bus
--
-- Mixer is an audio generator with inputs as channels and single output
module Live.Scene.Mixer
  ( Mixer (..)
  , MixerConfig (..)
  , MasterConfig (..)
  , ChannelConfig (..)
  , newMixer
  , FxParamId (..)
  , MixerChannels
  , newMixerChannels
  , appendMixerChannels
  , module X
  ) where

import Data.Boolean
import Data.Default
import Data.Maybe
import Csound.Core
import Safe (atMay)
import Live.Scene.Mixer.Config as X
import Live.Scene.Mixer.Fx (Bpm (..))
import Live.Scene.Mixer.Route
  ( FxParamId (..)
  , RouteDeps (..)
  , toMixerRoute
  , MixerRoute (..)
  , MixerRouteFx (..)
  )
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Live.Scene.Common (ChannelId (..))

data Mixer = Mixer
  -- audio
  { readMaster :: SE Sig2
  -- control
  , toggleChannelMute :: ChannelId -> SE ()
  , modifyChannelVolume :: ChannelId -> (Sig -> Sig) -> SE ()
  , modifyChannelSend :: ChannelId -> ChannelId -> (Sig -> Sig) -> SE ()
  , modifyMasterVolume :: (Sig -> Sig) -> SE ()
  , modifyFxParam :: FxParamId -> (Sig -> Sig) -> SE ()
  , clean :: SE ()
  }

newtype MixerChannels = MixerChannels [Channel]

newMixerChannels :: MixerConfig ChannelId -> SE MixerChannels
newMixerChannels config = MixerChannels <$> loadChannels config

appendMixerChannels :: MixerChannels -> ChannelId -> Sig2 -> SE ()
appendMixerChannels (MixerChannels channels) (ChannelId channelId) ins =
  mapM_ (\chan -> modifyRef chan.audio (+ ins)) (channels `atMay` channelId)

newMixer :: MixerConfig ChannelId -> MixerChannels -> SE Sig -> SE Mixer
newMixer config channels readBpm = do
  st <- initSt config channels
  let
    routeDeps = initRouteDeps st
  route <- toMixerRoute routeDeps (Bpm readBpm) config
  instrIds <- route.setupInstr
  let
    fxControls = route.fxControls instrIds
  route.launchInstr instrIds
  pure $ Mixer
    { readMaster = readMixSt st
    , modifyChannelVolume = modifyChannelVolumeSt st
    , modifyChannelSend = modifyChannelSendSt st
    , modifyMasterVolume = modifyMasterVolumeSt st
    , toggleChannelMute = toggleChannelMuteSt st
    , modifyFxParam = fxControls.modifyFxParam
    , clean = cleanSt st
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

initSt :: MixerConfig ChannelId -> MixerChannels -> SE St
initSt config (MixerChannels channels) = do
  master <- loadMaster (fromMaybe def config.master)
  pure St {..}
  where
    loadMaster masterConfig =
      Master
        <$> newCtrlRef (float masterConfig.volume)
        <*> newRef 0
        <*> pure masterConfig.gain

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
      pure Channel {..}

    initSends :: ChannelConfig ChannelId -> SE SendMap
    initSends channelConfig =
      case channelConfig.sends of
        Just sends -> SendMap . IntMap.fromList <$> mapM toSendMapItem sends
        Nothing -> pure $ SendMap mempty

    toSendMapItem :: SendConfig ChannelId -> SE (Int, Ref Sig)
    toSendMapItem send =
      fmap (send.channel.unChannelId, ) $ newCtrlRef (float send.gain)

readMixSt :: St -> SE Sig2
readMixSt St{..} = do
  volume <- readRef master.volume
  audio <- readRef master.audio
  pure $ withGain master.gain $ mul volume audio

writeChannelSt :: St -> ChannelId -> Sig2 -> SE ()
writeChannelSt st channelId audio =
  withChannel st channelId $ \channel -> do
    writeRef channel.audio audio

getChannel :: St -> ChannelId -> Maybe Channel
getChannel st (ChannelId n) = st.channels `atMay` n

withChannel :: St -> ChannelId -> (Channel -> SE ()) -> SE ()
withChannel st channelId f =
  mapM_ f (getChannel st channelId)

withChannelDef :: St -> ChannelId -> a -> (Channel -> SE a) -> SE a
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

modifyChannelSendSt :: St -> ChannelId -> ChannelId -> (Sig -> Sig) -> SE ()
modifyChannelSendSt st fromChannelId toChannelId f =
  withChannel st fromChannelId $ \channel ->
    mapM_ (\ref -> modifyRef ref f) (lookupSend toChannelId channel.sendGains)

modifyMasterVolumeSt :: St -> (Sig -> Sig) -> SE ()
modifyMasterVolumeSt st f =
  modifyRef st.master.volume f

withGain :: SigSpace a => Maybe Float -> a -> a
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
  pure $ withGain channel.gain (volume * mute)

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

      readMasterSt =
        readRef st.master.audio

      writeMasterSt input =
        writeRef st.master.audio input

      readChannelSendGainSt :: ChannelId -> ChannelId -> SE Sig
      readChannelSendGainSt channelId sendId =
        withChannelDef st channelId 1 $ \channel ->
          case lookupSend sendId channel.sendGains of
            Just sendRef -> readRef sendRef
            Nothing -> pure 0

