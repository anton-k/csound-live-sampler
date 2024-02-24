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
import Data.Boolean ((==*))
import Control.Monad
import Data.List qualified as List
import Data.Function qualified as Function
import Data.Either

runScene :: Config -> IO ()
runScene config =
  -- dacBy (setMa <> setTrace) {- writeCsd "tmp.csd" -} $ do
  writeCsdBy (setMa <> setDac) "tmp.csd" $ do
    scene <- loadScene config
    setupMidiFaders config scene
    toAudio config scene

data Scene = Scene
  { master :: Master
  , channels :: [Channel]
  , tracks :: [Track]
  , currentTrack :: Ref Sig
  }

data Master = Master
  { volume :: Ref Sig
  }

data Channel = Channel
  { volume :: Ref Sig
  , mute :: Ref Sig
  }

data Track = Track
  { instr :: InstrRef ()
  , audio :: Sig2
  }

data TrackId
data PartId

data Clip = Clip
  { track :: TrackId
  , part :: PartId
  }

-- | Completes crrently played clip and switches
-- to the next Clip
playClip :: Scene -> Clip -> SE ()
playClip = undefined

data ChanId
data FxId

-------------------------------------------------------------------------------------
-- audio playback

toAudio :: Config -> Scene -> SE Sig2
toAudio config scene = do
  play instrRef [Note 0 (-1) ()]
--   play instrRef [Note 5 1 ()]

  writeInitRef scene.currentTrack (fromRight undefined $ getInstrRefId instrRef)
  changeInstr scene
  applyMaster scene.master $ sum $ fmap (.audio) $ scene.tracks
  where
    instrRef = ((.instr) $ head $ scene.tracks)

-- Schema of multiple instrumentplay:
--
-- each track is rendered to it's own instrument
-- when instrument change event is triggered by midi
-- we lookup current played instrument and we turn it off
-- and start tto play next one

changeInstr :: Scene -> SE ()
changeInstr scene = do
  instr <- newProc $ \() -> do
    n <- notnum
    when1 (n ==* 24) $ do
      turnoff2_i track1.instr 0 0.01
      play track2.instr [Note 0 (-1) ()]
  global $ massign 1 instr
  where
    [track1, track2] = scene.tracks

trackInstr :: [Channel] -> TrackConfig -> SE Track
trackInstr channels track =
  uncurry Track <$> (newInstr PolyMix 0.01 $ const $ playTrack channels track)

playTrack :: [Channel] -> TrackConfig -> SE Sig2
playTrack channels track =
  sum <$> mapM playStemGroup (groupStemsByChannels channels track.stems)

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
  pure $ mul (vol * mute) $ sum $ fmap playStem stems

playStem :: Stem -> Sig2
playStem stem =
  mul (maybe 1 float stem.volume) (loopWav (fromString stem.file) 1)

applyMaster :: Master -> Sig2 -> SE Sig2
applyMaster master asig = do
  vol <- readRef master.volume
  pure $ mul vol asig

-------------------------------------------------------------------------------------
-- init scene

loadScene :: Config -> SE Scene
loadScene config = do
  master <- loadMaster
  channels <- loadChannels
  tracks <- loadTracks channels
  currentTrack <- initCurrentTrack
  pure $ Scene {..}
  where
    loadMaster =
      Master <$> newCtrlRef (float config.master.volume)

    loadChannels =
      mapM initChannel config.channels

    initChannel channelConfig = do
      volume <- newCtrlRef (float channelConfig.volume)
      mute <- newCtrlRef 1
      pure Channel {..}

    initCurrentTrack = newCtrlRef (-1)

    loadTracks channels = mapM (trackInstr channels) config.tracks

-------------------------------------------------------------------------------------
-- init midi controls

setupMidiFaders :: Config -> Scene -> SE ()
setupMidiFaders config scene = do
  setupMaster fadersMidi config.master scene.master
  setupChannels fadersMidi mutesMidi config.channels scene.channels
  where
    fadersMidi = config.controllers.faders
    mutesMidi = config.controllers.mutes

setupChannels :: FadersMidiConfig -> MutesMidiConfig -> [ChannelConfig] -> [Channel] -> SE ()
setupChannels fadersConfig (MutesMidiConfig mutesConfig) configs channels = do
  mapM_
    (\(chn, config, channel) -> setupChannel chn config channel)
    (zip3 midiChannels configs channels)
  initMutes $ zip (fmap int mutesConfig) (fmap (.mute) channels)
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

initMutes :: [(D, Ref Sig)] -> SE ()
initMutes mutes = do
  instr <- newProc (muteInstr mutes)
  global $ massign 1 instr

muteInstr :: [(D, Ref Sig)] -> () -> SE ()
muteInstr muteNotes = const $ do
  n <- notnum
  whens
    (fmap (uncurry $ toMuteCase n) muteNotes)
    (pure ())
  where
    toMuteCase :: D -> D -> Ref Sig -> (BoolD, SE ())
    toMuteCase notePressed noteId ref = (notePressed ==* noteId, go)
      where
        go = modifyInitRef ref $ \current -> ifB (current ==* 1) 0 1
