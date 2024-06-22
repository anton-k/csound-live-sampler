module Csound.Live.Sampler.Osc (
  Sampler (..),
  OscConfig (..),
  newSampler,
  ChannelId,
  TrackId,
) where

import Csound.Live.Sampler.Osc.Client
import Csound.Live.Sampler.Osc.Message (ChannelId, TrackId)
import Csound.Live.Sampler.Osc.Message qualified as Message

data Sampler = Sampler
  { setMasterVolume :: Double -> IO ()
  , setChannelVolume :: ChannelId -> Double -> IO ()
  , setTrack :: TrackId -> IO ()
  , shiftPart :: Int -> IO ()
  , nextPart :: IO ()
  , prevPart :: IO ()
  }

newSampler :: OscConfig -> IO Sampler
newSampler config = do
  client <- newOscClient config
  pure $
    Sampler
      { setMasterVolume = client.send . Message.setMasterVolumeMessage
      , setChannelVolume = \channelId volume -> client.send (Message.setChannelVolumeMessage channelId volume)
      , setTrack = \trackId -> client.send (Message.setTrackMessage trackId)
      , shiftPart = \steps -> client.send (Message.shiftPartMessage steps)
      , nextPart = client.send Message.nextPartMessage
      , prevPart = client.send Message.prevPartMessage
      }
