module Live.Scene.Midi
  ( setupMidi
  ) where

import Data.Bifunctor
import Live.Scene.Midi.Config
import Csound.Core hiding (Note)
import Csound.Core qualified as Core
import Data.Boolean
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Live.Scene.Mixer
import Live.Scene.Sampler
import Live.Scene.Sampler.Playlist qualified as Playlist
import Safe (atMay)
import Data.Containers.ListUtils qualified as List
import Data.Text (Text)
import Live.Scene.Common (ChannelId (..))

setupMidi :: Mixer -> Sampler -> MidiControllerConfig ChannelId -> SE ()
setupMidi mixer sampler config = do
  instrRef <- newProc actionsInstr
  play instrRef [Core.Note 0 (-1) ()]
  global $ massign 0 (instrRefFromNum 0 :: InstrRef ())
  where
    actionsInstr () = do
      note <- readNote
      mods <- setModifiers note modifierKeys
      mapM_ (setMidiActLink mixer sampler config.modifiers mods note) config.notes
      mapM_ (setMidiKnobLink mixer config.modifiers mods note) config.knobs

    modifierKeys :: [MidiModifier]
    modifierKeys = List.nubOrd $ mapMaybe (\x -> fromModifier x.when.channel =<< x.when.modifier) config.notes

    fromModifier :: Maybe MidiChannel -> NoteModifier -> Maybe MidiModifier
    fromModifier chan = \case
      NoteModifierKey n -> Just (MidiModifier n chan)
      NoteModifierName name -> Map.lookup name config.modifiers

setMidiActLink :: Mixer -> Sampler -> ModifierNames -> ModifierMap -> Note -> ActLink ChannelId -> SE ()
setMidiActLink mixer sampler modNames mods note link =
  when1 (note.isChange &&* toCond modNames mods note link.when) (mapM_ (toAct mixer sampler) link.act)

setMidiKnobLink :: Mixer -> ModifierNames -> ModifierMap -> Note -> KnobLink ChannelId -> SE ()
setMidiKnobLink mixer modNames mods note link =
  when1 (note.isChange &&* toKnobCond modNames mods note link.when) (mapM_ (toKnobAct mixer note) link.act)

readNote :: SE Note
readNote = do
  (status, chan, data1, data2) <- midiin
  let
    isChange = changed [status, chan, data1, data2] ==* 1
  pure Note {..}

data Note = Note
  { isChange :: BoolSig
  , status :: Sig
  , chan :: Sig
  , data1 :: Sig
  , data2 :: Sig
  }

newtype ModifierMap = ModifierMap (Map MidiModifier BoolSig)

noModifiersPressed :: Maybe MidiChannel -> ModifierMap -> BoolSig
noModifiersPressed mChan (ModifierMap mods) =
  notB (foldr (||*) false (Map.elems $ Map.filterWithKey modifierIsOnChannel mods))
  where
    modifierIsOnChannel modifier _ = modifier.channel == mChan

setModifiers :: Note -> [MidiModifier] -> SE ModifierMap
setModifiers note keys = do
  refs <- mapM (initModifier note) keys
  sigs <- mapM (fmap (==* 1) . readRef) refs
  pure $ ModifierMap (Map.fromList $ zip keys sigs)

initModifier :: Note -> MidiModifier -> SE (Ref Sig)
initModifier note modifier = do
  ref <- newCtrlRef 0
  when1 (note.isChange &&* withChan note modifier.channel (hasKey note modifier.key &&* isNoteOn note)) (writeRef ref 1)
  when1 (note.isChange &&* withChan note modifier.channel (hasKey note modifier.key &&* isNoteOff note)) (writeRef ref 0)
  pure ref

hasKey :: Note -> Int -> BoolSig
hasKey note key = note.data1 ==* int key

readKnobValue :: Note -> Sig
readKnobValue note = note.data2

isNoteOn :: Note -> BoolSig
isNoteOn note = note.status ==* 144

withChan :: Note -> Maybe MidiChannel -> BoolSig -> BoolSig
withChan note mChan = maybe id (\chan x -> hasChan note chan &&* x) mChan

hasChan :: Note -> MidiChannel -> BoolSig
hasChan note (MidiChannel channel) = note.chan ==* int channel

isNoteOff :: Note -> BoolSig
isNoteOff note = note.status ==* 128

type ModifierNames = Map Text MidiModifier

toCond :: ModifierNames -> ModifierMap -> Note -> MidiNote -> BoolSig
toCond modNames mods actualNote expectedNote =
  withChan actualNote expectedNote.channel $
      hasKey actualNote expectedNote.key
  &&* hasNoteType
  &&* hasModifier expectedNote.channel modNames mods expectedNote.modifier
  where
    hasNoteType =
      case expectedNote.press of
        Nothing -> isNoteOn actualNote
        Just MidiNoteOn -> isNoteOn actualNote
        Just MidiNoteOff -> isNoteOff actualNote

hasModifier :: Maybe MidiChannel -> ModifierNames -> ModifierMap -> Maybe NoteModifier -> BoolSig
hasModifier mChan modNames (ModifierMap mods) mModifier =
  case mModifier of
    Nothing -> noModifiersPressed mChan (ModifierMap mods)
    Just (NoteModifierKey n) -> fromMaybe false (Map.lookup (MidiModifier n mChan) mods)
    Just (NoteModifierName name) -> fromMaybe false $ do
      modifier <- Map.lookup name modNames
      Map.lookup modifier mods


toAct :: Mixer -> Sampler -> MidiAct ChannelId -> SE ()
toAct mixer sampler = \case
  ToggleMute n ->
    mixer.toggleChannelMute n
  SetTrack n -> withTrackId n $ \trackId ->
    Playlist.setTrack sampler.cursor trackId
  SetPart _n -> pure () -- TODO:  setPart sampler.cursor n
  ShiftTrack n -> sampler.cursor.modifyTrack (+ toSig (int n))
  ShiftPart n -> sampler.cursor.modifyPart (+ toSig (int n))
  PlayExtraClip column clip -> sampler.playExtraClip column clip

  where
    withTrackId n cont = mapM_ cont $ atMay sampler.getTrackIds (n - 1)

toKnobCond :: ModifierNames -> ModifierMap -> Note -> MidiKnob -> BoolSig
toKnobCond modNames mods actualNote expectedNote =
  withChan actualNote expectedNote.channel $
      hasKey actualNote expectedNote.key
  &&* hasModifier expectedNote.channel modNames mods expectedNote.modifier
  &&* isKnobStatus actualNote

isKnobStatus :: Note -> BoolSig
isKnobStatus note = note.status ==* 176

toKnobAct :: Mixer -> Note -> KnobWithRange ChannelId -> SE ()
toKnobAct mixer note knob =
  case knob.on of
    SetMasterVolume -> mixer.modifyMasterVolume $
      const (applyRange $ gainslider (readKnobValue note))
    SetChannelVolume n -> mixer.modifyChannelVolume n $
      const (applyRange $ gainslider (readKnobValue note))
    SetChannelSend config -> mixer.modifyChannelSend config.from config.to $
      const (applyRange $ readKnobValue note / 127)
    SetFxParam config -> mixer.modifyFxParam (toFxParamId config) $
      const (applyRange $ readKnobValue note / 127)
  where
    toFxParamId :: SetFxParamConfig -> FxParamId
    toFxParamId SetFxParamConfig{..} = FxParamId{..}

    applyRange :: Sig -> Sig
    applyRange = maybe id (rescaleUnitRangeTo . bimap float float) knob.range

rescaleUnitRangeTo :: (Sig, Sig) -> Sig -> Sig
rescaleUnitRangeTo (minVal, maxVal) x =
  minVal + (maxVal - minVal) * x
