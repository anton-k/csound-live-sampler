module Live.Scene.Midi.Config
  ( MidiControllerConfig (..)
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
  , SetChannelSendConfig (..)
  , SetFxParamConfig (..)
  , MidiKnobAct (..)
  , KnobLink (..)
  , KnobWithRange (..)
  , MidiChannel (..)
  ) where

import Data.Bifunctor
import Control.Applicative (Alternative (..))
import Data.Aeson (ToJSON, FromJSON, (.=), (.:))
import Data.Aeson qualified as Json
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Map.Strict (Map)
import Live.Scene.Sampler.Config (ColumnName (..), ClipName (..))
import Data.Aeson.TH qualified as Json

data MidiControllerConfig channel key = MidiControllerConfig
  { modifiers :: Maybe (Map Text (MidiModifier key))
  , keys :: Maybe (Map Text Int)
  , notes :: [ActLink channel key]
  , knobs :: [KnobLink channel key]
  }
  deriving (Functor)

instance Bifunctor MidiControllerConfig where
  first f (MidiControllerConfig mods keys notes knobs) =
    MidiControllerConfig mods keys (fmap (first f) notes) (fmap (first f) knobs)

  second f (MidiControllerConfig mods keys notes knobs) =
    MidiControllerConfig (fmap (fmap (fmap f)) mods) keys (fmap (second f) notes) (fmap (second f) knobs)


data MidiModifier key = MidiModifier
  { key :: key
  , channel :: Maybe MidiChannel
  }
  deriving (Eq, Ord, Functor)

-- | Note numbers for the mutes
newtype MutesMidiConfig = MutesMidiConfig [Int]
  deriving newtype (FromJSON, ToJSON)

-- | Note numbers for the track changes
newtype TrackChangesMidiConfig = TrackChangesMidiConfig [Int]
  deriving newtype (FromJSON, ToJSON)

newtype ShiftMidiConfig = ShiftMidiConfig Int
  deriving newtype (FromJSON, ToJSON)

data MidiNote key = MidiNote
  { key :: key
  , modifier :: Maybe NoteModifier
  , press :: Maybe MidiNoteType
  , channel :: Maybe MidiChannel
  }
  deriving (Functor)

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

data MidiAct channel
  = ToggleMute channel
  | SetTrack Int
  | SetPart Int
  | ShiftTrack Int
  | ShiftPart Int
  | PlayExtraClip ColumnName ClipName
  deriving (Functor)

instance ToJSON a => ToJSON (MidiAct a) where
  toJSON = \case
    ToggleMute n -> Json.object ["mute" .= n]
    SetTrack n -> Json.object ["track" .= n]
    SetPart n -> Json.object ["part" .= n]
    ShiftTrack n -> Json.object ["shiftTrack" .= n]
    ShiftPart n -> Json.object ["shiftPart" .= n]
    PlayExtraClip column clip -> Json.object [ "playClip" .= (column, clip)]

instance FromJSON a => FromJSON (MidiAct a) where
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

data ActLink channel key = ActLink
  { when :: MidiNote key
  , act :: [MidiAct channel]
  }
  deriving (Functor)

instance Bifunctor ActLink where
  first f (ActLink a b) = ActLink a (fmap (fmap f) b)
  second f (ActLink a b) = ActLink (fmap f a) b

data KnobLink channel key = KnobLink
  { when :: MidiKnob key
  , act :: [KnobWithRange channel]
  }
  deriving (Functor)

instance Bifunctor KnobLink where
  first f (KnobLink a b) = KnobLink a (fmap (fmap f) b)
  second f (KnobLink a b) = KnobLink (fmap f a) b

newtype MidiChannel = MidiChannel Int
  deriving newtype (FromJSON, ToJSON, Eq, Ord)

data MidiKnob key = MidiKnob
  { key :: key
  , modifier :: Maybe NoteModifier
  , channel :: Maybe MidiChannel
  }
  deriving (Functor)

data KnobWithRange channel = KnobWithRange
  { on :: MidiKnobAct channel
  , range :: Maybe (Float, Float)
  }
  deriving (Functor)

data MidiKnobAct channel
  = SetChannelVolume channel
  | SetMasterVolume
  | SetChannelSend (SetChannelSendConfig channel)
  | SetFxParam SetFxParamConfig
  deriving (Functor)

data SetChannelSendConfig channel = SetChannelSendConfig
  { from :: channel
  , to :: channel
  }
  deriving (Functor)

data SetFxParamConfig = SetFxParamConfig
  { name :: Text
  , param :: Text
  }

-- JSON instances

$(Json.deriveJSON Json.defaultOptions ''MidiModifier)
$(Json.deriveJSON Json.defaultOptions ''SetChannelSendConfig)
$(Json.deriveJSON Json.defaultOptions ''SetFxParamConfig)
$(Json.deriveJSON Json.defaultOptions ''MidiNote)

instance ToJSON a => ToJSON (MidiKnobAct a) where
  toJSON = \case
    SetChannelVolume n -> Json.object ["channelVolume" .= n]
    SetMasterVolume -> Json.String "masterVolume"
    SetChannelSend config -> Json.object ["channelSend" .= config]
    SetFxParam config -> Json.object ["fxParam" .= config]

instance FromJSON a => FromJSON (MidiKnobAct a) where
  parseJSON = \case
    Json.String "masterVolume" -> pure SetMasterVolume
    Json.Object obj ->
      let
        getValue cons name = cons <$> (obj .: name)
      in
            getValue SetChannelVolume "channelVolume"
        <|> getValue SetChannelSend "channelSend"
        <|> getValue SetFxParam "fxParam"
    _ -> fail "Failed to parse MidiKnobAct"

$(Json.deriveJSON Json.defaultOptions ''KnobWithRange)
$(Json.deriveJSON Json.defaultOptions ''MidiKnob)
$(Json.deriveJSON Json.defaultOptions ''KnobLink)
$(Json.deriveJSON Json.defaultOptions ''ActLink)
$(Json.deriveJSON Json.defaultOptions ''MidiControllerConfig)
