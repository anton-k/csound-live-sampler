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
import Live.Scene.Fx (FxDeps (..), newFxs, FxParams)
import Live.Scene.Fx qualified as Fx (modifyFxParam)
import Live.Scene.Fx.Config
import Data.Text (Text)

data Mixer = Mixer
  -- audio
  { audio :: Gen
  -- control
  , toggleChannelMute :: ChannelId -> SE ()
  , modifyChannelVolume :: ChannelId -> (Sig -> Sig) -> SE ()
  , modifyMasterVolume :: (Sig -> Sig) -> SE ()
  , modifyFxParam :: FxParamId -> (Sig -> Sig) -> SE ()
  }

data FxParamId = FxParamId
  { name :: Text
  , param :: Text
  }

getChannelSize :: Mixer -> Int
getChannelSize mixer =
  length mixer.audio.inputs

newMixer :: MixerConfig -> [FxConfig] -> SE Sig -> SE Mixer
newMixer config fxConfig readBpm = do
  st <- initSt config
  updateMasterInstrRef <- newProc (\() -> writeChannelsToMaster st)
  fxParams <- newFxs (initFxDeps st readBpm) fxConfig
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
          , update = play updateMasterInstrRef [Note 0 (-1) ()] -- writeChannelsToMaster st
          }
    , modifyChannelVolume = modifyChannelVolumeSt st
    , modifyMasterVolume = modifyMasterVolumeSt st
    , toggleChannelMute = toggleChannelMuteSt st
    , modifyFxParam = modifyFxParamSt fxParams
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
  , fx :: Sig2 -> SE Sig2
  }

data Channel = Channel
  { volume :: Ref Sig
  , mute :: Ref Sig
  , gain :: Maybe Float
  , audio :: Ref Sig2
  , fx :: Sig2 -> SE Sig2
  }

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
      pure Channel {..}

{-
toFx :: (FxConfig -> Bool) -> [FxConfig] -> Sig2 -> SE Sig2
toFx hasInput fxConfigs ain =
  List.foldl' (>=>) pure (fmap unitToFun masterFxs) ain
  where
    masterFxs :: [FxUnit]
    masterFxs = do
      fx <- filter hasInput fxConfigs
      fmap (.fx) fx.chain

toMasterFx :: [FxConfig] -> Sig2 -> SE Sig2
toMasterFx = toFx isMasterFx
  where
    isMasterFx :: FxConfig -> Bool
    isMasterFx config =
      case config.input of
        MasterFx -> True
        _ -> False

toChannelFx :: ChannelId -> [FxConfig] -> Sig2 -> SE Sig2
toChannelFx (ChannelId n) = toFx isChannel
  where
    isChannel :: FxConfig -> Bool
    isChannel config =
      case config.input of
        ChannelFx (ChannelFxConfig channelId) -> channelId == n
        _ -> False
-}

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

writeChannelsToMaster :: St -> SE ()
writeChannelsToMaster st = do
  mapM_ writeToMaster st.channels
  applyMasterFx
  where
    writeToMaster :: Channel -> SE ()
    writeToMaster channel = do
      volume <- readRef channel.volume
      mute <- readRef channel.mute
      audio <- readRef channel.audio
      let
        channelAudio = withGain channel.gain $ mul (volume * mute) audio
      modifyRef st.master.audio (+ channelAudio)

    applyMasterFx :: SE ()
    applyMasterFx =
      writeRef st.master.audio =<< st.master.fx =<< readRef st.master.audio

getChannel :: St -> ChannelId -> Maybe Channel
getChannel st (ChannelId n) = st.channels `atMay` n

withChannel :: St -> ChannelId -> (Channel -> SE ()) -> SE ()
withChannel st channelId f =
  mapM_ f (getChannel st channelId)

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

withGain :: Maybe Float -> Sig2 -> Sig2
withGain mValue audio =
  case mValue of
    Nothing -> audio
    Just value -> mul (float value) audio

-------------------------------------------------------------------------------------
-- FXs

initFxDeps :: St -> SE Sig -> FxDeps
initFxDeps st readBpm =
  FxDeps
    { readMaster = readRef st.master.audio
    , writeMaster = writeRef st.master.audio
    , readChannel = \n -> maybe (pure 0) (readRef . (.audio)) (st.channels `atMay` n)
    , writeChannel = \n -> writeChannelSt st (ChannelId n)
    , readBpm
    }

modifyFxParamSt :: FxParams -> FxParamId -> (Sig -> Sig) -> SE ()
modifyFxParamSt fxParams paramId f =
  Fx.modifyFxParam fxParams paramId.name paramId.param f
