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
  , MidiModifier (..)
  , MidiKnob (..)
  , SetFxParamConfig (..)
  , MidiKnobAct (..)
  , KnobLink (..)
  , KnobWithRange (..)
  , MidiChannel (..)
  ) where

import Control.Applicative (Alternative (..))
import Data.Aeson (ToJSON, FromJSON, (.=), (.:))
import Data.Aeson qualified as Json
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Map.Strict (Map)
import Live.Scene.Sampler.Config (ColumnName (..), ClipName (..))
import Data.Aeson.TH qualified as Json

data ControllerConfig = ControllerConfig
  { modifiers :: Map Text MidiModifier
  , notes :: [ActLink]
  , knobs :: [KnobLink]
  }

data MidiModifier = MidiModifier
  { key :: Int
  , channel :: Maybe MidiChannel
  }
  deriving (Eq, Ord)

data FadersMidiConfig = FadersMidiConfig
  { master :: Int
  , channels :: [Int]
  }

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
  , channel :: Maybe MidiChannel
  }

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
  | PlayExtraClip ColumnName ClipName

instance ToJSON MidiAct where
  toJSON = \case
    ToggleMute n -> Json.object ["mute" .= n]
    SetTrack n -> Json.object ["track" .= n]
    SetPart n -> Json.object ["part" .= n]
    ShiftTrack n -> Json.object ["shiftTrack" .= n]
    ShiftPart n -> Json.object ["shiftPart" .= n]
    PlayExtraClip column clip -> Json.object [ "playClip" .= (column, clip)]

instance FromJSON MidiAct where
  parseJSON = Json.withObject "MidiAct" $ \obj ->
    let
      getInt cons name = cons <$> (obj .: name)

      getPair cons name =
        (\x -> case x of
            [a, b] -> pure $ cons a b
            _ -> fail "Failed to read pair of values"
        ) =<< (obj .: name)
    in
          getInt ToggleMute "mute"
      <|> getInt SetTrack "track"
      <|> getInt SetPart "part"
      <|> getInt ShiftPart "shiftTrack"
      <|> getInt ShiftPart "shiftPart"
      <|> getPair (\a b -> PlayExtraClip (ColumnName a) (ClipName b)) "playClip"
      <|> fail "Failed to parse midi action"

data ActLink = ActLink
  { when :: MidiNote
  , act :: [MidiAct]
  }

data KnobLink = KnobLink
  { when :: MidiKnob
  , act :: [KnobWithRange]
  }

newtype MidiChannel = MidiChannel Int
  deriving newtype (FromJSON, ToJSON, Eq, Ord)

data MidiKnob = MidiKnob
  { key :: Int
  , modifier :: Maybe NoteModifier
  , channel :: Maybe MidiChannel
  }

data KnobWithRange = KnobWithRange
  { on :: MidiKnobAct
  , range :: Maybe (Float, Float)
  }

data MidiKnobAct
  = SetChannelVolume Int
  | SetMasterVolume
  | SetFxParam SetFxParamConfig

data SetFxParamConfig = SetFxParamConfig
  { name :: Text
  , param :: Text
  }

-- JSON instances

$(Json.deriveJSON Json.defaultOptions ''MidiModifier)
$(Json.deriveJSON Json.defaultOptions ''SetFxParamConfig)
$(Json.deriveJSON Json.defaultOptions ''MidiNote)

instance ToJSON MidiKnobAct where
  toJSON = \case
    SetChannelVolume n -> Json.object ["channelVolume" .= n]
    SetMasterVolume -> Json.String "masterVolume"
    SetFxParam config -> Json.object ["fxParam" .= config]

instance FromJSON MidiKnobAct where
  parseJSON = \case
    Json.String "masterVolume" -> pure SetMasterVolume
    Json.Object obj ->
      let
        getValue cons name = cons <$> (obj .: name)
      in
            getValue SetChannelVolume "channelVolume"
        <|> getValue SetFxParam "fxParam"
    _ -> fail "Failed to parse MidiKnobAct"

$(Json.deriveJSON Json.defaultOptions ''KnobWithRange)
$(Json.deriveJSON Json.defaultOptions ''MidiKnob)
$(Json.deriveJSON Json.defaultOptions ''KnobLink)
$(Json.deriveJSON Json.defaultOptions ''ActLink)
$(Json.deriveJSON Json.defaultOptions ''ControllerConfig)
