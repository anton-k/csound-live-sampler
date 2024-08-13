-- | Common interfaces
module Action
  ( ChannelId
  , TrackId
  , Scene
  , Mixer
  , Sampler
  , Info
  , FxParamId
  , SendId
  ) where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe)

type ChannelId = Int

type TrackId = Int

type Scene =
  { mixer :: Mixer
  , sampler :: Sampler
  , info :: Info
  }

type FxParamId =
  { channel :: Maybe ChannelId
  , name :: String
  , param :: String
  }

type SendId =
  { from :: ChannelId
  , to :: ChannelId
  }

type Mixer =
  { setMasterVolume :: Number -> Effect Unit
  , setChannelVolume :: ChannelId -> Number -> Effect Unit
  , setFxParam :: FxParamId -> Number -> Effect Unit
  , setSendFx :: SendId -> Number -> Effect Unit
  }

type Sampler =
  { setTrack :: TrackId -> Effect Unit
  , shiftTrack :: Int -> Effect Unit
  , shiftPart :: Int -> Effect Unit
  }

type Info =
  { getCurrentPart :: Effect Unit
  }
