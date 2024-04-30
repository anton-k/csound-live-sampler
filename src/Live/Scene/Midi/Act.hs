module Live.Scene.Midi.Act
  ( setMidiActions
  ) where

import Live.Scene.Midi.Config
import Csound.Core hiding (Note)
import Csound.Core qualified as Core
import Data.Boolean
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Live.Scene.Mixer
import Live.Scene.Sampler
import Live.Scene.Sampler.Playlist qualified as Playlist
import Safe (atMay)
import Control.Monad
import Data.Containers.ListUtils qualified as List
import Data.Text (Text)

setMidiActions :: Mixer -> Sampler -> ControllerConfig -> SE ()
setMidiActions mixer sampler config = do
  instrRef <- newProc actionsInstr
  play instrRef [Core.Note 0 (-1) ()]
  global $ massign 0 (instrRefFromNum 0 :: InstrRef ())
  where
    actionsInstr () = do
      note <- readNote
      mods <- setModifiers note modifierKeys
      mapM_ (setMidiActLink mixer sampler config.modifiers mods note) config.notes
      mapM_ (setMidiKnobLink mixer config.modifiers mods note) config.knobs

    modifierKeys :: [Int]
    modifierKeys = List.nubOrd $ mapMaybe (fromModifier <=< (.when.modifier)) config.notes

    fromModifier = \case
      NoteModifierKey n -> Just n
      NoteModifierName name -> Map.lookup name config.modifiers

setMidiActLink :: Mixer -> Sampler -> ModifierNames -> ModifierMap -> Note -> ActLink -> SE ()
setMidiActLink mixer sampler modNames mods note link =
  when1 (note.isChange &&* toCond modNames mods note link.when) (mapM_ (toAct mixer sampler) link.act)

setMidiKnobLink :: Mixer -> ModifierNames -> ModifierMap -> Note -> KnobLink -> SE ()
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

newtype ModifierMap = ModifierMap (IntMap BoolSig)

noModifiersPressed :: ModifierMap -> BoolSig
noModifiersPressed (ModifierMap mods) = notB (foldr (||*) false (IntMap.elems mods))

setModifiers :: Note -> [Int] -> SE ModifierMap
setModifiers note keys = do
  refs <- mapM (initModifier note) keys
  sigs <- mapM (fmap (==* 1) . readRef) refs
  pure $ ModifierMap (IntMap.fromList $ zip keys sigs)

initModifier :: Note -> Int -> SE (Ref Sig)
initModifier note key = do
  ref <- newCtrlRef 0
  when1 (note.isChange &&* hasKey note key &&* isNoteOn note) (writeRef ref 1)
  when1 (note.isChange &&* hasKey note key &&* isNoteOff note) (writeRef ref 0)
  pure ref

hasKey :: Note -> Int -> BoolSig
hasKey note key = note.data1 ==* int key

readKnobValue :: Note -> Sig
readKnobValue note = note.data2

isNoteOn :: Note -> BoolSig
isNoteOn note = note.status ==* 144

isNoteOff :: Note -> BoolSig
isNoteOff note = note.status ==* 128

type ModifierNames = Map Text Int

toCond :: ModifierNames -> ModifierMap -> Note -> MidiNote -> BoolSig
toCond modNames mods actualNote expectedNote =
      hasKey actualNote expectedNote.key
  &&* hasNoteType
  &&* hasModifier modNames mods expectedNote.modifier
  where
    hasNoteType =
      case expectedNote.press of
        Nothing -> isNoteOn actualNote
        Just MidiNoteOn -> isNoteOn actualNote
        Just MidiNoteOff -> isNoteOff actualNote

hasModifier :: ModifierNames -> ModifierMap -> Maybe NoteModifier -> BoolSig
hasModifier modNames (ModifierMap mods) mModifier =
  case mModifier of
    Nothing -> noModifiersPressed (ModifierMap mods)
    Just (NoteModifierKey n) -> fromMaybe false (IntMap.lookup n mods)
    Just (NoteModifierName name) -> fromMaybe false $ do
      key <- Map.lookup name modNames
      IntMap.lookup key mods


toAct :: Mixer -> Sampler -> MidiAct -> SE ()
toAct mixer sampler = \case
  ToggleMute n ->
    mixer.toggleChannelMute (ChannelId (n - 1))
  SetTrack n -> withTrackId n $ \trackId ->
    Playlist.setTrack sampler.cursor trackId
  SetPart _n -> pure () -- TODO:  setPart sampler.cursor n
  ShiftTrack n -> sampler.cursor.modifyTrack (+ toSig (int n))
  ShiftPart n -> sampler.cursor.modifyPart (+ toSig (int n))
  where
    withTrackId n cont = mapM_ cont $ atMay sampler.getTrackIds (n - 1)

toKnobCond :: ModifierNames -> ModifierMap -> Note -> MidiKnob -> BoolSig
toKnobCond modNames mods actualNote expectedNote =
      hasKey actualNote expectedNote.key
  &&* hasModifier modNames mods expectedNote.modifier
  &&* isKnobStatus actualNote

isKnobStatus :: Note -> BoolSig
isKnobStatus note = note.status ==* 176

toKnobAct :: Mixer -> Note -> KnobWithRange -> SE ()
toKnobAct mixer note knob =
  case knob.on of
    SetMasterVolume -> mixer.modifyMasterVolume $
      const (gainslider (readKnobValue note))
    SetChannelVolume n -> mixer.modifyChannelVolume (ChannelId (n - 1)) $
      const (gainslider (readKnobValue note))
