module Live.Scene.Midi
  ( setupMidi
  ) where

import Data.Boolean
import Live.Config
import Live.Scene.Mixer
import Live.Scene.Sampler
import Csound.Core
import Live.Scene.Midi.Config
import Live.Scene.Midi.Mixer
import Live.Scene.Midi.Sampler

setupMidi :: Config -> Mixer -> Sampler -> SE ()
setupMidi config mixer sampler = do
  shift <- newShift
  setupMidiFaders config.controllers config.mixer mixer
  runMidiInstr $ initMidiInstr config mixer sampler shift

data MidiInstr = MidiInstr (D -> SE ())

initMidiInstr :: Config -> Mixer -> Sampler -> Shift -> MidiInstr
initMidiInstr config mixer sampler shift = MidiInstr $ \n -> do
  muteInstr mixer (getShift shift) mutes n
  changeTrackInstr config.controllers.trackChanges sampler (getShift shift) n
  shiftInstr config.controllers.shift shift n
  where
    MutesMidiConfig mutesConfig = config.controllers.mutes
    mutes = zip (fmap int mutesConfig) mixer.audio.inputs

runMidiInstr :: MidiInstr -> SE ()
runMidiInstr (MidiInstr body) = do
  instrId <- newProc $ \() -> body =<< notnum
  global $ massign 1 instrId

-- * shift button control

data Shift = Shift (Ref Sig)

newShift :: SE Shift
newShift = Shift <$> newCtrlRef 0

shiftInstr :: ShiftMidiConfig -> Shift -> D -> SE ()
shiftInstr (ShiftMidiConfig n) (Shift shiftRef) note =
  when1 (note ==* int n) $
    writeRef shiftRef (linsegr [1] 0.1 0)

getShift :: Shift -> SE BoolD
getShift (Shift ref) = (==* 1) <$> readInitRef ref
