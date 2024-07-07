-- | Common interfaces
module Action
  ( ChannelId
  , TrackId
  , Scene
  , Mixer
  , Sampler
  , Info
  ) where

import Prelude
import Effect (Effect)

type ChannelId = Int

type TrackId = Int

type Scene =
  { mixer :: Mixer
  , sampler :: Sampler
  , info :: Info
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

type Info =
  { getCurrentPart :: Effect Unit
  }
