module Live.Scene.Midi.Config
  ( ControllerConfig (..)
  , FadersMidiConfig (..)
  , MutesMidiConfig (..)
  , TrackChangesMidiConfig (..)
  , ShiftMidiConfig (..)
  , ActLink (..)
  , MidiAct (..)
  , MidiNote (..)
  , MidiNoteType (..)
  , NoteModifier (..)
  , MidiKnob (..)
  , MidiKnobAct (..)
  , KnobLink (..)
  , KnobWithRange (..)
  ) where

import Control.Applicative (Alternative (..))
import Data.Aeson (ToJSON, FromJSON, (.=), (.:))
import Data.Aeson qualified as Json
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Map.Strict (Map)

data ControllerConfig = ControllerConfig
  { modifiers :: Map Text Int
  , notes :: [ActLink]
  , knobs :: [KnobLink]
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

data MidiNote = MidiNote
  { key :: Int
  , modifier :: Maybe NoteModifier
  , press :: Maybe MidiNoteType
  }
  deriving (Generic, FromJSON, ToJSON)

data MidiNoteType = MidiNoteOn | MidiNoteOff

instance ToJSON MidiNoteType where
  toJSON = \case
    MidiNoteOn -> "on"
    MidiNoteOff -> "off"

instance FromJSON MidiNoteType where
  parseJSON = Json.withText "MidiNoteType" $ \case
    "on" -> pure MidiNoteOn
    "off" -> pure MidiNoteOff
    other -> fail $ Text.unpack ("Failed to parse: " <> other)

data NoteModifier
  = NoteModifierKey Int
  | NoteModifierName Text

instance ToJSON NoteModifier where
  toJSON = \case
    NoteModifierKey n -> Json.toJSON n
    NoteModifierName str -> Json.toJSON str

instance FromJSON NoteModifier where
  parseJSON = \case
    Json.String txt -> pure (NoteModifierName txt)
    Json.Number n -> pure (NoteModifierKey $ floor n)
    _ -> fail "Failed to parse Note modifier, use integer or string"

data MidiAct
  = ToggleMute Int
  | SetTrack Int
  | SetPart Int
  | ShiftTrack Int
  | ShiftPart Int

instance ToJSON MidiAct where
  toJSON = \case
    ToggleMute n -> Json.object ["mute" .= n]
    SetTrack n -> Json.object ["track" .= n]
    SetPart n -> Json.object ["part" .= n]
    ShiftTrack n -> Json.object ["shiftTrack" .= n]
    ShiftPart n -> Json.object ["shiftPart" .= n]

instance FromJSON MidiAct where
  parseJSON = Json.withObject "MidiAct" $ \obj ->
    let
      getInt cons name = cons <$> (obj .: name)
    in
          getInt ToggleMute "mute"
      <|> getInt SetTrack "track"
      <|> getInt SetPart "part"
      <|> getInt ShiftPart "shiftTrack"
      <|> getInt ShiftPart "shiftPart"
      <|> fail "Failed to parse midi action"

data ActLink = ActLink
  { when :: MidiNote
  , act :: [MidiAct]
  }
  deriving (Generic, FromJSON, ToJSON)

data KnobLink = KnobLink
  { when :: MidiKnob
  , act :: [KnobWithRange]
  }
  deriving (Generic, FromJSON, ToJSON)

data MidiKnob = MidiKnob
  { key :: Int
  , modifier :: Maybe NoteModifier
  }
  deriving (Generic, FromJSON, ToJSON)

data KnobWithRange = KnobWithRange
  { on :: MidiKnobAct
  , range :: Maybe (Float, Float)
  }
  deriving (Generic, FromJSON, ToJSON)

data MidiKnobAct
  = SetChannelVolume Int
  | SetMasterVolume

instance ToJSON MidiKnobAct where
  toJSON = \case
    SetChannelVolume n -> Json.object ["channelVolume" .= n]
    SetMasterVolume -> Json.String "masterVolume"

instance FromJSON MidiKnobAct where
  parseJSON = \case
    Json.String "masterVolume" -> pure SetMasterVolume
    Json.Object obj ->
      let
        getInt cons name = cons <$> (obj .: name)
      in
        getInt SetChannelVolume "channelVolume"
    _ -> fail "Failed to parse MidiKnobAct"

