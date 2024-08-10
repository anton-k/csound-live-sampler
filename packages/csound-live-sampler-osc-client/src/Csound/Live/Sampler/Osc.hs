module Csound.Live.Sampler.Osc (
  Sampler (..),
  OscConfig (..),
  newSampler,
  ChannelId,
  TrackId,
  AudioInputId,
  FxName,
  ParamName,
) where

import Csound.Live.Sampler.Osc.Client
import Csound.Live.Sampler.Osc.Message (
  AudioInputId,
  ChannelId,
  FxName,
  ParamName,
  TrackId,
 )
import Csound.Live.Sampler.Osc.Message qualified as Message

data Sampler = Sampler
  -- mixer
  { setMasterVolume :: Double -> IO ()
  , setChannelVolume :: ChannelId -> Double -> IO ()
  , toggleMute :: ChannelId -> IO ()
  , setMasterFxParam :: FxName -> ParamName -> Double -> IO ()
  , setChannelFxParam :: ChannelId -> FxName -> ParamName -> Double -> IO ()
  , setChannelSend :: ChannelId -> ChannelId -> Double -> IO ()
  , -- sampler
    setTrack :: TrackId -> IO ()
  , shiftPart :: Int -> IO ()
  , nextPart :: IO ()
  , prevPart :: IO ()
  , nextTrack :: IO ()
  , prevTrack :: IO ()
  , -- audio card
    setAudioInputGain :: AudioInputId -> Double -> IO ()
  }

newSampler :: OscConfig -> IO Sampler
newSampler config = do
  client <- newOscClient config
  pure $
    Sampler
      { setMasterVolume = client.send . Message.setMasterVolumeMessage
      , setChannelVolume = \channelId volume -> client.send (Message.setChannelVolumeMessage channelId volume)
      , toggleMute = \channelId -> client.send (Message.toggleMuteMessage channelId)
      , setChannelSend = \channelFrom channelTo gain -> client.send (Message.setChannelSendMessage channelFrom channelTo gain)
      , setTrack = \trackId -> client.send (Message.setTrackMessage trackId)
      , shiftPart = \steps -> client.send (Message.shiftPartMessage steps)
      , nextPart = client.send Message.nextPartMessage
      , prevPart = client.send Message.prevPartMessage
      , nextTrack = client.send Message.nextTrackMessage
      , prevTrack = client.send Message.prevTrackMessage
      , setMasterFxParam = \unit param value -> client.send (Message.setFxParamMessage Nothing unit param value)
      , setChannelFxParam = \channelId unit param value -> client.send (Message.setFxParamMessage (Just channelId) unit param value)
      , setAudioInputGain = \inputId value -> client.send (Message.setAudioInputGainMessage inputId value)
      }
