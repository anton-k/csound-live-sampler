module Live.Scene.Midi
  ( setupMidi
  ) where

import Data.Boolean
import Live.Config
import Live.Scene.Mixer
import Live.Scene.Sampler
import Csound.Core
import Live.Scene.Midi.Config

data MidiControl = MidiControl
  { shift :: Shift
  }

setupMidi :: Config -> Mixer -> Sampler -> SE ()
setupMidi config mixer sampler = do
  shift <- newShift
  runMidiInstr $ initMidiInstr config mixer sampler shift

data Shift = Shift (Ref Sig)

newShift :: SE Shift
newShift = Shift <$> newCtrlRef 0

getShift :: Shift -> SE BoolD
getShift (Shift ref) = (==* 1) <$> readInitRef ref

data MidiInstr = MidiInstr (D -> SE ())

setupMidiFaders :: Config -> Mixer -> SE ()
setupMidiFaders config mixer = do
  setupMaster mixer fadersMidi config.mixer.master
  setupChannels mixer fadersMidi config.mixer.channels
  where
    fadersMidi = config.controllers.faders

setupChannels :: Mixer -> FadersMidiConfig -> [ChannelConfig] -> SE ()
setupChannels mixer fadersConfig configs = do
  mapM_
    (\(chn, config, channel) -> setupChannel mixer chn config channel)
    (zip3 midiChannels configs channels)
  where
    midiChannels = int <$> fadersConfig.channels
    channels = mixer.audio.inputs

setupChannel :: Mixer -> D -> ChannelConfig -> ChannelId -> SE ()
setupChannel mixer chn config channel = do
  initFader chn config.volume (mixer.modifyChannelVolume channel . const)

setupMaster :: Mixer -> FadersMidiConfig -> MasterConfig -> SE ()
setupMaster mixer fadersConfig config =
  initFader (int fadersConfig.master) config.volume (mixer.modifyMasterVolume . const)

initFader :: D -> Float -> (Sig -> SE ()) -> SE ()
initFader chn initVal write = do
  initc7  (CtrlInit 1 chn (float initVal))
  write kVol
  where
    kVol = gainslider $ kr $ ctrl7 1 chn 0 127

muteInstr :: Mixer -> Shift -> [(D,ChannelId)] -> D -> SE ()
muteInstr mixer shiftRef muteNotes n = do
  isShift <- getShift shiftRef
  whens
    (fmap (uncurry $ toMuteCase isShift n) muteNotes)
    (pure ())
  where
    toMuteCase :: BoolD -> D -> D -> ChannelId -> (BoolD, SE ())
    toMuteCase isShift notePressed noteId channelId =
      (notB isShift &&* notePressed ==* noteId, go)
      where
        go = mixer.toggleChannelMute channelId

changeInstr :: TrackChangesMidiConfig -> Sampler -> Shift -> D -> SE ()
changeInstr (TrackChangesMidiConfig trackButtons) sampler shiftRef = \n -> do
  isShift <- getShift shiftRef
  whens (zipWith (instrCase isShift n) trackButtons sampler.getTrackIds) (pure ())
  where
    instrCase isShift buttonMidi buttonTrack trackId = (cond, body)
      where
        cond = isShift &&* (buttonMidi ==* int buttonTrack)

        body = sampler.setTrack trackId

shiftInstr :: ShiftMidiConfig -> Shift -> D -> SE ()
shiftInstr (ShiftMidiConfig n) (Shift shiftRef) note =
  when1 (note ==* int n) $
    writeRef shiftRef (linsegr [1] 0.1 0)

initMidiInstr :: Config -> Mixer -> Sampler -> Shift -> MidiInstr
initMidiInstr config mixer sampler shift = MidiInstr $ \n -> do
  muteInstr mixer shift mutes n
  changeInstr config.controllers.trackChanges sampler shift n
  shiftInstr config.controllers.shift shift n
  where
    MutesMidiConfig mutesConfig = config.controllers.mutes
    mutes = zip (fmap int mutesConfig) mixer.audio.inputs

runMidiInstr :: MidiInstr -> SE ()
runMidiInstr (MidiInstr body) = do
  instrId <- newProc $ \() -> body =<< notnum
  global $ massign 1 instrId
