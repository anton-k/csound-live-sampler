module Live.Scene.Midi.Config (
  MidiControllerConfig (..),
  mapMidiControllerConfig,
  MutesMidiConfig (..),
  TrackChangesMidiConfig (..),
  ShiftMidiConfig (..),
  ActLink (..),
  MidiAct (..),
  MidiNote (..),
  MidiNoteType (..),
  NoteModifier (..),
  MidiModifier (..),
  MidiKnob (..),
  mapKnobLink,
  SetChannelSendConfig (..),
  SetFxParamConfig (..),
  MidiKnobAct (..),
  KnobLink (..),
  KnobWithRange (..),
  MidiChannel (..),
) where

import Control.Applicative (Alternative (..))
import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import Data.Aeson qualified as Json
import Data.Aeson.TH qualified as Json
import Data.Bifunctor
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Live.Scene.Sampler.Config (ClipName (..), ColumnName (..))

data MidiControllerConfig audioInput channel key = MidiControllerConfig
  { modifiers :: Maybe (Map Text (MidiModifier key))
  , keys :: Maybe (Map Text Int)
  , notes :: [ActLink channel key]
  , knobs :: [KnobLink audioInput channel key]
  }
  deriving (Functor)

mapMidiControllerConfig ::
  (a1 -> a2) ->
  (b1 -> b2) ->
  (c1 -> c2) ->
  MidiControllerConfig a1 b1 c1 ->
  MidiControllerConfig a2 b2 c2
mapMidiControllerConfig f g h (MidiControllerConfig modsA keysA notesA knobsA) =
  MidiControllerConfig modsB keysB notesB knobsB
  where
    modsB = fmap (fmap (fmap h)) modsA
    keysB = keysA
    notesB = fmap (bimap g h) notesA
    knobsB = fmap (mapKnobLink f g h) knobsA

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

instance (ToJSON a) => ToJSON (MidiAct a) where
  toJSON = \case
    ToggleMute n -> Json.object ["mute" .= n]
    SetTrack n -> Json.object ["track" .= n]
    SetPart n -> Json.object ["part" .= n]
    ShiftTrack n -> Json.object ["shiftTrack" .= n]
    ShiftPart n -> Json.object ["shiftPart" .= n]
    PlayExtraClip column clip -> Json.object ["playClip" .= (column, clip)]

instance (FromJSON a) => FromJSON (MidiAct a) where
  parseJSON = Json.withObject "MidiAct" $ \obj ->
    let
      getInt cons name = cons <$> (obj .: name)

      getPair cons name =
        ( \x -> case x of
            [a, b] -> pure $ cons a b
            _ -> fail "Failed to read pair of values"
        )
          =<< (obj .: name)
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

data KnobLink audioInput channel key = KnobLink
  { when :: MidiKnob key
  , act :: [KnobWithRange audioInput channel]
  }
  deriving (Functor)

mapKnobLink ::
  (a1 -> a2) ->
  (b1 -> b2) ->
  (c1 -> c2) ->
  KnobLink a1 b1 c1 ->
  KnobLink a2 b2 c2
mapKnobLink f g h (KnobLink whenA actA) =
  KnobLink whenB actB
  where
    whenB = fmap h whenA
    actB = fmap (bimap f g) actA

newtype MidiChannel = MidiChannel Int
  deriving newtype (FromJSON, ToJSON, Eq, Ord)

data MidiKnob key = MidiKnob
  { key :: key
  , modifier :: Maybe NoteModifier
  , channel :: Maybe MidiChannel
  }
  deriving (Functor)

data KnobWithRange audioInput channel = KnobWithRange
  { on :: MidiKnobAct audioInput channel
  , range :: Maybe (Float, Float)
  }
  deriving (Functor)

instance Bifunctor KnobWithRange where
  bimap f g (KnobWithRange on range) = KnobWithRange (bimap f g on) range

data MidiKnobAct audioInput channel
  = SetChannelVolume channel
  | SetMasterVolume
  | SetChannelSend (SetChannelSendConfig channel)
  | SetFxParam SetFxParamConfig
  | SetAudioInputGain audioInput
  deriving (Functor)

instance Bifunctor MidiKnobAct where
  bimap f g = \case
    SetChannelVolume channel -> SetChannelVolume (g channel)
    SetMasterVolume -> SetMasterVolume
    SetChannelSend config -> SetChannelSend (fmap g config)
    SetFxParam config -> SetFxParam config
    SetAudioInputGain audioInput -> SetAudioInputGain (f audioInput)

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

instance (ToJSON a, ToJSON b) => ToJSON (MidiKnobAct a b) where
  toJSON = \case
    SetChannelVolume n -> Json.object ["channelVolume" .= n]
    SetMasterVolume -> Json.String "masterVolume"
    SetChannelSend config -> Json.object ["channelSend" .= config]
    SetFxParam config -> Json.object ["fxParam" .= config]
    SetAudioInputGain audioInput -> Json.object ["audioInputGain" .= audioInput]

instance (FromJSON a, FromJSON b) => FromJSON (MidiKnobAct a b) where
  parseJSON = \case
    Json.String "masterVolume" -> pure SetMasterVolume
    Json.Object obj ->
      let
        getValue cons name = cons <$> (obj .: name)
       in
        getValue SetChannelVolume "channelVolume"
          <|> getValue SetChannelSend "channelSend"
          <|> getValue SetFxParam "fxParam"
          <|> getValue SetFxParam "audioInputGain"
    _ -> fail "Failed to parse MidiKnobAct"

$(Json.deriveJSON Json.defaultOptions ''KnobWithRange)
$(Json.deriveJSON Json.defaultOptions ''MidiKnob)
$(Json.deriveJSON Json.defaultOptions ''KnobLink)
$(Json.deriveJSON Json.defaultOptions ''ActLink)
$(Json.deriveJSON Json.defaultOptions ''MidiControllerConfig)
