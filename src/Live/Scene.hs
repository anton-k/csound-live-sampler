module Live.Scene
  ( Scene
  , runScene
  , loadScene
  , TrackId
  , PartId
  , Clip
  ) where

import Data.Maybe
import Live.Config
import Csound.Core
import Safe (atMay)
import Data.Boolean ((==*), (/=*), (>*))
import Control.Monad
import Data.List qualified as List
import Data.Function qualified as Function
import Data.Either

runScene :: Config -> IO ()
runScene config =
  dacBy (setMa <> setTrace) {- writeCsd "tmp.csd" -} $ do
  -- writeCsdBy (setMa <> setDac) "tmp.csd" $ do
    scene <- loadScene config
    setupMidiFaders config scene
    toAudio scene

data Scene = Scene
  { master :: Master
  , channels :: [Channel]
  , tracks :: [Track]
  , currentTrack :: CurrentTrack
  , shift :: Shift
  , midiInstr :: MidiInstr
  }

data MidiInstr = MidiInstr (D -> SE ())

data CurrentTrack = CurrentTrack (Ref D)

data Shift = Shift (Ref Sig)

getShift :: Shift -> SE BoolD
getShift (Shift ref) = (==* 1) <$> readInitRef ref

setCurrentTrack :: CurrentTrack -> InstrRef () -> SE ()
setCurrentTrack (CurrentTrack ref) instrRef =
  case getInstrRefId instrRef of
    Right n -> writeRef ref n
    Left _ -> pure ()

getCurrentTrack :: CurrentTrack -> SE D
getCurrentTrack (CurrentTrack ref) =
  readRef ref

data Master = Master
  { volume :: Ref Sig
  , audio :: Ref Sig2
  , gain :: Maybe Float
  }

data Channel = Channel
  { volume :: Ref Sig
  , mute :: Ref Sig
  , gain :: Maybe Float
  }

data Track = Track
  { instr :: InstrRef ()
  }

data TrackId
data PartId

data Clip = Clip
  { track :: TrackId
  , part :: PartId
  }

-------------------------------------------------------------------------------------
-- audio playback

toAudio :: Scene -> SE Sig2
toAudio scene = do
  runMidiInstr scene.midiInstr
  applyMaster scene.master

-- Schema of multiple instrumentplay:
--
-- each track is rendered to it's own instrument
-- when instrument change event is triggered by midi
-- we lookup current played instrument and we turn it off
-- and start tto play next one

trackInstr :: Ref Sig2 -> [Channel] -> TrackConfig -> SE Track
trackInstr masterRef channels track =
  fmap Track $ newProc $ \() -> do
    audio <- playTrack channels track
    writeRef masterRef audio

playTrack :: [Channel] -> TrackConfig -> SE Sig2
playTrack channels track =
  withGain track.gain . sum
    <$> mapM playStemGroup (groupStemsByChannels channels track.stems)

-- | Group of stems that belong to the same channel
data StemGroup = StemGroup
  { channel :: Channel
  , stems :: [Stem]
  }

groupStemsByChannels :: [Channel] -> [Stem] -> [StemGroup]
groupStemsByChannels channels stems =
  mapMaybe fromGroup $ groupOn (.channel) $ List.sortOn (.channel) stems
  where
    groupOn f = List.groupBy ((==) `Function.on` f)

    fromGroup = \case
      [] -> Nothing
      x:xs -> fmap (\chan -> StemGroup chan (x:xs)) $ channels `atMay` (x.channel - 1)

playStemGroup :: StemGroup -> SE Sig2
playStemGroup (StemGroup channel stems) = do
  vol <- readRef channel.volume
  mute <- readRef channel.mute
  pure $ withGain channel.gain $ mul (vol * mute) $ sum $ fmap playStem stems

playStem :: Stem -> Sig2
playStem stem =
  withGain stem.gain $ mul (maybe 1 float stem.volume) (loopWav (fromString stem.file) 1)

applyMaster :: Master -> SE Sig2
applyMaster master = do
  vol <- readRef master.volume
  audio <- readRef master.audio
  writeRef master.audio 0
  pure $ withGain master.gain $ mul vol audio

-------------------------------------------------------------------------------------
-- init scene

loadScene :: Config -> SE Scene
loadScene config = do
  master <- loadMaster
  channels <- loadChannels
  tracks <- loadTracks master channels
  currentTrack <- initCurrentTrack
  shift <- initShift
  let
    midiInstr = initMidiInstr config currentTrack channels tracks shift
  pure $ Scene {..}
  where
    loadMaster =
      Master
        <$> newCtrlRef (float config.master.volume)
        <*> newRef 0
        <*> pure config.master.gain

    loadChannels =
      mapM initChannel config.channels

    initChannel channelConfig = do
      volume <- newCtrlRef (float channelConfig.volume)
      mute <- newCtrlRef 1
      let
        gain = channelConfig.gain
      pure Channel {..}

    initCurrentTrack = CurrentTrack <$> (newCtrlRef (-1))

    initShift = Shift <$> newCtrlRef 0

    loadTracks master channels = mapM (trackInstr master.audio channels) config.tracks

-------------------------------------------------------------------------------------
-- init midi controls

setupMidiFaders :: Config -> Scene -> SE ()
setupMidiFaders config scene = do
  setupMaster fadersMidi config.master scene.master
  setupChannels fadersMidi config.channels scene.channels
  where
    fadersMidi = config.controllers.faders

setupChannels :: FadersMidiConfig -> [ChannelConfig] -> [Channel] -> SE ()
setupChannels fadersConfig configs channels = do
  mapM_
    (\(chn, config, channel) -> setupChannel chn config channel)
    (zip3 midiChannels configs channels)
  where
    midiChannels = int <$> fadersConfig.channels

setupChannel :: D -> ChannelConfig -> Channel -> SE ()
setupChannel chn config channel = do
  initFader chn config.volume channel.volume

setupMaster :: FadersMidiConfig -> MasterConfig -> Master -> SE ()
setupMaster fadersConfig config master =
  initFader (int fadersConfig.master) config.volume master.volume

initFader :: D -> Float -> Ref Sig -> SE ()
initFader chn initVal ref = do
  initc7  (CtrlInit 1 chn (float initVal))
  writeRef ref kVol
  where
    kVol = gainslider $ kr $ ctrl7 1 chn 0 127

muteInstr :: Shift -> [(D, Ref Sig)] -> D -> SE ()
muteInstr shiftRef muteNotes n = do
  isShift <- getShift shiftRef
  whens
    (fmap (uncurry $ toMuteCase isShift n) muteNotes)
    (pure ())
  where
    toMuteCase :: BoolD -> D -> D -> Ref Sig -> (BoolD, SE ())
    toMuteCase isShift notePressed noteId ref = (notB isShift &&* notePressed ==* noteId, go)
      where
        go = modifyInitRef ref $ \current -> ifB (current ==* 1) 0 1

changeInstr :: TrackChangesMidiConfig -> CurrentTrack -> [Track] -> Shift -> D -> SE ()
changeInstr (TrackChangesMidiConfig trackButtons) currentTrackRef tracks shiftRef = \n -> do
  isShift <- getShift shiftRef
  whens (zipWith (instrCase isShift n) trackButtons tracks) (pure ())
  where
    instrCase isShift buttonMidi buttonTrack track = (cond, body)
      where
        cond = isShift &&* (buttonMidi ==* int buttonTrack)

        body = do
          currentTrack <- getCurrentTrack currentTrackRef
          when1 (currentTrack /=* getInstrRefIdNum track.instr) $ do
            setCurrentTrack currentTrackRef track.instr
            when1 (currentTrack >* 0) $
              turnoff2_i (instrRefFromNum @() currentTrack) 0 0.05
            play track.instr [Note 0 (-1) ()]

shiftInstr :: ShiftMidiConfig -> Shift -> D -> SE ()
shiftInstr (ShiftMidiConfig n) (Shift shiftRef) note =
  when1 (note ==* int n) $
    writeRef shiftRef (linsegr [1] 0.1 0)

initMidiInstr :: Config -> CurrentTrack -> [Channel] -> [Track] -> Shift -> MidiInstr
initMidiInstr config currentTrack channels tracks shift = MidiInstr $ \n -> do
  muteInstr shift mutes n
  changeInstr config.controllers.trackChanges currentTrack tracks shift n
  shiftInstr config.controllers.shift shift n
  where
    MutesMidiConfig mutesConfig = config.controllers.mutes
    mutes = zip (fmap int mutesConfig) (fmap (.mute) channels)

runMidiInstr :: MidiInstr -> SE ()
runMidiInstr (MidiInstr body) = do
  instrId <- newProc $ \() -> body =<< notnum
  global $ massign 1 instrId

-------------------------------------------------------------------------------------
-- utils

withGain :: Maybe Float -> Sig2 -> Sig2
withGain mValue audio =
  case mValue of
    Nothing -> audio
    Just value -> mul (float value) audio

