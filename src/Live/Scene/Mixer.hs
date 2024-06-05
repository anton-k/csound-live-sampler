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
  , getChannelSize
  , FxParamId (..)
  , module X
  ) where

import Data.Boolean
import Csound.Core
import Safe (atMay)
import Live.Scene.Gen as X
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

data Mixer = Mixer
  -- audio
  { audio :: Gen
  -- control
  , toggleChannelMute :: ChannelId -> SE ()
  , modifyChannelVolume :: ChannelId -> (Sig -> Sig) -> SE ()
  , modifyMasterVolume :: (Sig -> Sig) -> SE ()
  , modifyFxParam :: FxParamId -> (Sig -> Sig) -> SE ()
  }

getChannelSize :: Mixer -> Int
getChannelSize mixer =
  length mixer.audio.inputs

newMixer :: MixerConfig -> SE Sig -> SE Mixer
newMixer config readBpm = do
  st <- initSt config
  let
    routeDeps = initRouteDeps st
  route <- toMixerRoute routeDeps (Bpm readBpm) config
  instrIds <- route.setupInstr
  let
    fxControls = route.fxControls instrIds
  pure $ Mixer
    { audio =
        Gen
          { read = \(ChannelId n) ->
              if n == 0
                then readMixSt st
                else pure 0
          , write = writeChannelSt st
          , inputs = ChannelId <$> [0 .. (length config.channels - 1)]
          , outputs = [ChannelId 0]
          , update = route.launchInstr instrIds
          }
    , modifyChannelVolume = modifyChannelVolumeSt st
    , modifyMasterVolume = modifyMasterVolumeSt st
    , toggleChannelMute = toggleChannelMuteSt st
    , modifyFxParam = fxControls.modifyFxParam
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
  , _fx :: Sig2 -> SE Sig2 -- TODO
  }

data Channel = Channel
  { volume :: Ref Sig
  , mute :: Ref Sig
  , gain :: Maybe Float
  , audio :: Ref Sig2
  , fx :: Sig2 -> SE Sig2
  , sendGains :: SendMap
  }

newtype SendMap = SendMap (IntMap (Ref Sig))

lookupSend :: Int -> SendMap -> Maybe (Ref Sig)
lookupSend channelId (SendMap refs) =
  IntMap.lookup channelId refs

initSt :: MixerConfig -> SE St
initSt config = do
  master <- loadMaster
  channels <- loadChannels
  pure St {..}
  where
    loadMaster =
      Master
        <$> newCtrlRef (float config.master.volume)
        <*> newRef 0
        <*> pure config.master.gain
        <*> pure pure -- (toMasterFx fxConfigs)

    loadChannels =
      mapM initChannel config.channels

    initChannel channelConfig = do
      volume <- newCtrlRef (float channelConfig.volume)
      mute <- newCtrlRef 1
      let
        gain = channelConfig.gain
        fx = pure -- toChannelFx (ChannelId channelId) fxConfigs
      audio <- newRef 0
      sendGains <- initSends channelConfig
      pure Channel {..}

    initSends :: ChannelConfig -> SE SendMap
    initSends channelConfig =
      case channelConfig.sends of
        Just sends -> SendMap . IntMap.fromList <$> mapM toSendMapItem sends
        Nothing -> pure $ SendMap mempty

    toSendMapItem :: SendConfig -> SE (Int, Ref Sig)
    toSendMapItem send =
      fmap (send.channel, ) $ newCtrlRef (float send.gain)

readMixSt :: St -> SE Sig2
readMixSt St{..} = do
  volume <- readRef master.volume
  audio <- readRef master.audio
  writeRef master.audio 0
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

modifyMasterVolumeSt :: St -> (Sig -> Sig) -> SE ()
modifyMasterVolumeSt st f =
  modifyRef st.master.volume f

withGain :: SigSpace a => Maybe Float -> a -> a
withGain mValue audio =
  case mValue of
    Nothing -> audio
    Just value -> mul (float value) audio

-------------------------------------------------------------------------------------
-- Routes

initRouteDeps :: St -> RouteDeps
initRouteDeps st =
  RouteDeps
    { readChannel = \n -> withChannelDef st (ChannelId n) 0 (readRef . (.audio))
    , readChannelVolume = \n -> withChannelDef st (ChannelId n) 1 (\channel -> withGain channel.gain <$> (readRef channel.volume))
    , readChannelSendGain = readChannelSendGainSt
    , writeChannel = \n -> writeChannelSt st (ChannelId n)
    , appendChannel = \n -> appendChannelSt (ChannelId n)
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

      readChannelSendGainSt channelId sendId =
        withChannelDef st (ChannelId channelId) 1 $ \channel ->
          case lookupSend sendId channel.sendGains of
            Just sendRef -> readRef sendRef
            Nothing -> pure 0

