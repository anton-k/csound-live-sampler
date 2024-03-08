module Live.Scene.Midi.Config
  ( ControllerConfig (..)
  , FadersMidiConfig (..)
  , MutesMidiConfig (..)
  , TrackChangesMidiConfig (..)
  , ShiftMidiConfig (..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

data ControllerConfig = ControllerConfig
  { faders :: FadersMidiConfig
  , mutes :: MutesMidiConfig
  , shift :: ShiftMidiConfig
  , trackChanges :: TrackChangesMidiConfig
  }
  deriving (Generic, FromJSON, ToJSON)

data FadersMidiConfig = FadersMidiConfig
  { master :: Int
  , channels :: [Int]
  }
  deriving (Generic, FromJSON, ToJSON)

-- | Note numbers for the mutes
newtype MutesMidiConfig = MutesMidiConfig [Int]
  deriving newtype (FromJSON, ToJSON)

-- | Note numbers for the track changes
newtype TrackChangesMidiConfig = TrackChangesMidiConfig [Int]
  deriving newtype (FromJSON, ToJSON)

newtype ShiftMidiConfig = ShiftMidiConfig Int
  deriving newtype (FromJSON, ToJSON)


