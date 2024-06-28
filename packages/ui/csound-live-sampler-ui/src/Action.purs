-- | Common interfaces
module Action
  ( ChannelId
  , TrackId
  , Scene
  , Mixer
  , Sampler
  ) where

import Prelude
import Effect (Effect)

type ChannelId = Int

type TrackId = Int

type Scene =
  { mixer :: Mixer
  , sampler :: Sampler
  }

type Mixer =
  { setMasterVolume :: Number -> Effect Unit
  , setChannelVolume :: ChannelId -> Number -> Effect Unit
  }

type Sampler =
  { setTrack :: TrackId -> Effect Unit
  , shiftTrack :: Int -> Effect Unit
  , shiftPart :: Int -> Effect Unit
  }

