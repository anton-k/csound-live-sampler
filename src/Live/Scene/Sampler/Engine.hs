-- | Engine can play instruments in the loop syncronized on BPM.
--
-- Note that engine can play any instrument which has single
-- argument: skip start time
module Live.Scene.Sampler.Engine
  ( Clip (..)
  , Part (..)
  , StartTime
  , ClipInstr
  , Engine (..)
  , newEngine
  ) where

import Data.Boolean
import Csound.Core

type StartTime = D
type ClipInstr = InstrRef StartTime

data Clip = Clip
  { bpm :: D
  , start :: D
  , changeRate :: D
  , beatSize :: D
  , timeSize :: D
  }

data Part = Part
  { clip :: Clip
  , track :: ClipInstr
  }

data Engine = Engine
  { setPart :: Part -> SE ()
  , start :: SE ()
  , stop :: SE ()
  }

newEngine :: SE Engine
newEngine = do
  st <- initSt
  pure $
    Engine
      { setPart = setPartSt st
      , start = startSt st
      , stop = stopSt st
      }

-- * internal state

data St = St
  { main :: InstrRef ()
  , bpm :: Bpm
  , current :: ClipSt
  , next :: ClipSt
  }

newtype Bpm = Bpm (Ref Sig)

data ClipSt = ClipSt
  { track :: Ref Sig
  , bpm :: Ref Sig
  , startTime :: Ref Sig
  , timeSize :: Ref Sig
  , change :: Counter
  , loop :: Counter
  }

copyClipTo :: ClipSt -> ClipSt -> SE ()
copyClipTo from to =
  mapM_ copy
    [ (.track)
    , (.bpm)
    , (.startTime)
    , (.timeSize)
    , (.change.size)
    , (.change.step)
    , (.loop.size)
    , (.loop.step)
    ]
  where
    copy f = writeRef (f to) =<< readRef (f from)

data Counter = Counter
  { size :: Ref Sig
  , step :: Ref Sig
  }

initCounter :: Sig -> SE Counter
initCounter maxSize =
  Counter
    <$> newCtrlRef maxSize
    <*> newCtrlRef maxSize

nextCount :: Counter -> SE BoolSig
nextCount counter = do
  modifyRef counter.step (\x -> x - 1)
  current <- readRef counter.step
  when1 (current ==* 0) $
    writeRef counter.step =<< readRef counter.size
  pure (current ==* 0)

initSt :: SE St
initSt = do
  bpm <- Bpm  <$> newCtrlRef 120
  current <- initClipSt
  next <- initClipSt
  main <- newProc (\() -> mainInstr bpm current next)
  pure St {..}
  where
    initClipSt = do
      track <- newCtrlRef (-1)
      bpm <- newCtrlRef 120
      startTime <- newCtrlRef 0
      timeSize <- newCtrlRef 0.5
      change <- initCounter 1
      loop <- initCounter 1
      pure ClipSt {..}

mainInstr :: Bpm -> ClipSt -> ClipSt -> SE ()
mainInstr bpm current next =
  periodic bpm $ do
    isBoundary <- updateCounters current
    onChanges isBoundary bpm current next

onChanges :: IsBoundary -> Bpm -> ClipSt -> ClipSt -> SE ()
onChanges boundary bpm current next = do
  isClip <- isClipChange current next
  whens
    [ (boundary.isChange &&* isClip, onChange bpm current next)
    , (boundary.isLoop, onLoop current)
    ]
    (pure ())

isClipChange :: ClipSt -> ClipSt -> SE BoolSig
isClipChange current next = do
  currentTrackId <- readTrackId current
  nextTrackId <- readTrackId next
  pure (notB $ equalTrackId currentTrackId nextTrackId)

onChange :: Bpm -> ClipSt -> ClipSt -> SE ()
onChange bpm current next = do
  currentTrackId <- readTrackId current
  nextTrackId <- readTrackId next

  when1 (notB $ equalTrackId currentTrackId nextTrackId) $ do
    stopClip current
    scheduleClip bpm next
    copyClipTo next current

stopClip :: ClipSt -> SE ()
stopClip clip = do
  track <- readRef clip.track
  when1 (track >=* 0) $
    turnoff2 (instrRefFromNum @D $ toD track) 0 0.05

scheduleClip :: Bpm -> ClipSt -> SE ()
scheduleClip (Bpm bpmRef) clip = do
  writeRef bpmRef =<< readRef clip.bpm
  TrackId track start dur <- readTrackId clip
  play (instrRefFromNum $ toD track) [Note 0 (toD dur) (toD start)]

data TrackId = TrackId
  { track :: Sig
  , start :: Sig
  , dur :: Sig
  }

equalTrackId :: TrackId -> TrackId -> BoolSig
equalTrackId (TrackId a1 b1 c1) (TrackId a2 b2 c2) =
  a1 ==* a2 &&* b1 ==* b2 &&* c1 ==* c2

readTrackId :: ClipSt -> SE TrackId
readTrackId clip =
  TrackId
    <$> readRef clip.track
    <*> readRef clip.startTime
    <*> readRef clip.timeSize

onLoop :: ClipSt -> SE ()
onLoop current = do
  track <- readRef current.track
  startTime <- readRef current.startTime
  dur <- readRef current.timeSize
  play (instrRefFromNum $ toD track) [Note 0 (toD dur) (toD startTime)]

data IsBoundary = IsBoundary
  { isChange :: BoolSig
  , isLoop :: BoolSig
  }

updateCounters :: ClipSt -> SE IsBoundary
updateCounters clip = do
  isChange <- nextCount clip.change
  isLoop <- nextCount clip.loop
  pure IsBoundary {..}

periodic :: Bpm -> SE () -> SE ()
periodic (Bpm bpmRef) onTick = do
  bpm <- readRef bpmRef
  when1 (metro (toAbsTimeRate bpm 1) ==* 1)
    onTick

setPartSt :: St -> Part -> SE ()
setPartSt st part = do
  write (.track) (getInstrRefIdNum part.track)
  write (.bpm) part.clip.bpm
  write (.startTime) part.clip.start
  write (.timeSize) part.clip.timeSize
  write (.change.size) part.clip.changeRate
  write (.change.step) part.clip.changeRate
  write (.loop.size) part.clip.beatSize
  write (.loop.step) part.clip.beatSize
  where
    write f = writeInitRef (f st.next)

startSt :: St -> SE ()
startSt st =
  play st.main [Note 0 (-1) ()]

stopSt :: St -> SE ()
stopSt st =
  turnoff2_i st.main 0 0.05

-- | Converts beats to seconds
toAbsTimeRate :: Sig -> Sig -> Sig
toAbsTimeRate bpm beats = bpm / (60 * beats)
