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
  , module X
  ) where

import Data.Boolean
import Csound.Core
import Safe (atMay)
import Live.Scene.Gen as X
import Live.Scene.Mixer.Config as X

data Mixer = Mixer
  -- audio
  { audio :: Gen
  -- control
  , toggleChannelMute :: ChannelId -> SE ()
  , modifyChannelVolume :: ChannelId -> (Sig -> Sig) -> SE ()
  , modifyMasterVolume :: (Sig -> Sig) -> SE ()
  }

getChannelSize :: Mixer -> Int
getChannelSize mixer =
  length mixer.audio.inputs

newMixer :: MixerConfig -> SE Mixer
newMixer config = do
  st <- initSt config
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
          , update = pure ()
          }
    , modifyChannelVolume = modifyChannelVolumeSt st
    , modifyMasterVolume = modifyMasterVolumeSt st
    , toggleChannelMute = toggleChannelMuteSt st
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

    loadChannels =
      mapM initChannel config.channels

    initChannel channelConfig = do
      volume <- newCtrlRef (float channelConfig.volume)
      mute <- newCtrlRef 1
      let
        gain = channelConfig.gain
      pure Channel {..}

readMixSt :: St -> SE Sig2
readMixSt St{..} = do
  volume <- readRef master.volume
  audio <- readRef master.audio
  writeRef master.audio 0
  pure $ withGain master.gain $ mul volume audio

writeChannelSt :: St -> ChannelId -> Sig2 -> SE ()
writeChannelSt st channelId audio =
  withChannel st channelId $ \channel -> do
    volume <- readRef channel.volume
    mute <- readRef channel.mute
    let
      channelAudio = withGain channel.gain $ mul (volume * mute) audio
    modifyRef st.master.audio (+ channelAudio)

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
