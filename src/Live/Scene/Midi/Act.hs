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
      actionInstr <- initActionInstr mixer sampler
      note <- readNote
      mods <- setModifiers note modifierKeys
      mapM_ (setMidiActLink sampler config.modifiers mods actionInstr note) config.notes

    modifierKeys :: [Int]
    modifierKeys = List.nubOrd $ mapMaybe (fromModifier <=< (.when.modifier)) config.notes

    fromModifier = \case
      NoteModifierKey n -> Just n
      NoteModifierName name -> Map.lookup name config.modifiers

setMidiActLink :: Sampler -> ModifierNames -> ModifierMap -> ActionInstr -> Note -> ActLink -> SE ()
setMidiActLink sampler modNames mods instrs note link =
  when1 (note.isChange &&* toCond modNames mods note link.when) (mapM_ (toAct sampler instrs) link.act)

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

isNoteOn :: Note -> BoolSig
isNoteOn note = note.status ==* 144

isNoteOff :: Note -> BoolSig
isNoteOff note = note.status ==* 128

type ModifierNames = Map Text Int

toCond :: ModifierNames -> ModifierMap -> Note -> MidiNote -> BoolSig
toCond modNames (ModifierMap mods) actualNote expectedNote =
      hasKey actualNote expectedNote.key
  &&* hasNoteType
  &&* hasModifier
  where
    hasNoteType =
      case expectedNote.press of
        Nothing -> isNoteOn actualNote
        Just MidiNoteOn -> isNoteOn actualNote
        Just MidiNoteOff -> isNoteOff actualNote

    hasModifier =
      case expectedNote.modifier of
        Nothing -> noModifiersPressed (ModifierMap mods)
        Just (NoteModifierKey n) -> fromMaybe false (IntMap.lookup n mods)
        Just (NoteModifierName name) -> fromMaybe false $ do
          key <- Map.lookup name modNames
          IntMap.lookup key mods

data ActionInstr = ActionInstr
  { toggleMutes :: [InstrRef ()]
  , setTrack :: InstrRef TrackId
  , shiftTrack :: InstrRef D
  , shiftPart :: InstrRef D
  }

initActionInstr :: Mixer -> Sampler -> SE ActionInstr
initActionInstr mixer sampler = do
  toggleMutes <- mapM initMute [0 .. getChannelSize mixer - 1]

  setTrack <- newProc $ \trackId -> do
    Playlist.setTrack sampler.cursor trackId
    turnoffSelf 0 0

  shiftTrack <- newProc $ \n -> do
    sampler.cursor.modifyTrack (+ n)
    turnoffSelf 0 0

  shiftPart <- newProc $ \n -> do
    sampler.cursor.modifyPart (+ n)
    turnoffSelf 0 0

  pure ActionInstr {..}
  where
    initMute n = newProc $ \() -> do
      mixer.toggleChannelMute (ChannelId n)
      turnoffSelf 0 0

toAct :: Sampler -> ActionInstr -> MidiAct -> SE ()
toAct sampler instrs = \case
  ToggleMute n -> mapM_  (\instr -> play instr [Core.Note 0 1 ()]) (atMay instrs.toggleMutes (n - 1) )
  SetTrack n -> withTrackId n $ \trackId -> play instrs.setTrack [Core.Note 0 1 trackId]
  SetPart _n -> pure () -- TODO:  setPart sampler.cursor n
  ShiftTrack n -> play instrs.shiftTrack [Core.Note 0 1 (int n)]
  ShiftPart n -> play instrs.shiftPart [Core.Note 0 1 (int n)]
  where
    withTrackId n cont = mapM_ cont $ atMay sampler.getTrackIds (n - 1)
