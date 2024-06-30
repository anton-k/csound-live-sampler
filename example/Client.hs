{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Client where

import Csound.Live.Sampler.Osc
import System.IO.Unsafe

sampler :: Sampler
sampler = unsafePerformIO $ newSampler (OscConfig "127.0.0.1" 3333)

(=:) :: Item -> Double -> IO ()
(=:) = \case
  ChannelVolume index -> sampler.setChannelVolume index
  Track -> sampler.setTrack . round
  _ -> error "Unsupported"

data Item
  = MasterVolume
  | ChannelVolume ChannelId
  | Track
  | FxParam FxName ParamName
  | Send ChannelId ChannelId

data Channel = Channel
  { index :: ChannelId
  , volume :: Double -> IO ()
  , send :: ChannelId -> Item
  }

initChannel :: ChannelId -> Channel
initChannel index =
  Channel
    { index
    , volume = \val -> sampler.setChannelVolume index val
    , send = \to -> Send index to
    }

track index = sampler.setTrack index

next = sampler.nextPart
prev = sampler.prevPart

nextTrack = sampler.nextTrack
prevTrack = sampler.prevPart

drums, perc, pad, solo, core :: Channel
drums = initChannel 1
perc = initChannel 2
bass = initChannel 3
pad = initChannel 4
solo = initChannel 5
core = initChannel 6
