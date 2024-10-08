{-| Engine can play instruments in the loop syncronized on BPM.

Note that engine can play any instrument which has single
argument: skip start time

TOOD: interesting idea: we can make it more generic if we
use csoundf opcode scoreline thus we can have just three arguments for clip:
* instr id
* arguments as a string
* duration

or even we can keep everything in single score, but this way we can not stop it
-}
module Live.Scene.Sampler.Engine (
  Clip (..),
  Part (..),
  ColumnId,
  ClipInstr,
  Engine (..),
  newEngine,
  ExtraClipSize,
  toAbsTimeRate,
) where

import Control.Monad
import Csound.Core
import Data.Boolean
import Prelude hiding ((<*))

-- | Single argument is start time of the loop in audio file
type ClipInstr = Sig -- InstrRef D

data Clip = Clip
  { bpm :: Sig
  , start :: Sig
  , changeRate :: Sig
  , beatSize :: Sig
  , timeSize :: Sig
  , measure :: Sig
  , trackIndex :: Sig
  , partIndex :: Sig
  , numOfParts :: Sig
  , nextAction :: Sig
  -- ^
  -- next actions:
  --   0 - loop
  --   1 - play next
  --   2 - stop
  }

playLoop, playNext, stopPlayback :: D
playLoop = 0
playNext = 1
stopPlayback = 2

instance FromTuple Clip where
  fromTuple (Clip a b c d e f g h i j) = fromTuple ((a, b, c, d), (e, f, g, h, i, j))

instance Tuple Clip where
  tupleArity = 10
  tupleRates = replicate 10 Kr
  defTuple = Clip 0 0 0 0 0 0 0 0 0 0
  toTuple = (\((a, b, c, d), (e, f, g, h, i, j)) -> Clip a b c d e f g h i j) . toTuple

data Part = Part
  { clip :: Clip
  , track :: ClipInstr
  }

instance FromTuple Part where
  fromTuple (Part a b) = fromTuple (a, b)

instance Tuple Part where
  tupleArity = tupleArity @(Clip, ClipInstr)
  tupleRates = tupleRates @(Clip, ClipInstr)
  defTuple = Part defTuple 0
  toTuple = (\(a, b) -> Part a b) . toTuple

type ColumnId = Sig

data Engine = Engine
  { setTrackPart :: Part -> SE ()
  , setExtraClipPart :: ColumnId -> Part -> SE ()
  , start :: SE ()
  , stop :: SE ()
  , readBpm :: SE Sig
  , readTicks :: SE Sig
  , currentBeat :: SE Sig
  , readClip :: SE Clip
  , readIsMainClipChange :: SE Sig
  }

-- | Number of columns in extra clips
type ExtraClipSize = Int

newEngine :: GetNextPart -> ExtraClipSize -> SE Engine
newEngine getNextPart extraColumnSize = do
  st <- initSt getNextPart extraColumnSize
  pure $
    Engine
      { setTrackPart = setTrackPartSt st
      , setExtraClipPart = setClipPartSt st
      , start = startSt st
      , stop = stopSt st
      , readBpm = readBpmSt st
      , readTicks = readTicksSt st
      , currentBeat = currentBeatSt st
      , readClip = readClipSt st
      , readIsMainClipChange = readRef st.isMainClipChange
      }

-- * internal state

data St = St
  { main :: InstrRef ()
  , bpm :: Bpm
  , current :: ClipRef
  , next :: ClipRef
  , extraClips :: Maybe ExtraClips
  , isTick :: Ref Sig
  , beatCounter :: Counter Ref
  , isMainClipChange :: Ref Sig
  }

data ExtraClips = ExtraClips
  { currents :: ClipColumn
  , nexts :: ClipColumn
  , size :: Int
  }

initExtraClips :: ExtraClipSize -> SE ExtraClips
initExtraClips size = do
  currents <- initClipSt newColumn
  nexts <- initClipSt newColumn
  pure ExtraClips{..}
  where
    newColumn value = Column <$> fillGlobalCtrlArr [size] (replicate size value)

newtype Bpm = Bpm (Ref Sig)

type ClipRef = ClipSt Ref

type ClipColumn = ClipSt Column

data ClipSt f = ClipSt
  { track :: f Sig
  , bpm :: f Sig
  , startTime :: f Sig
  , timeSize :: f Sig
  , change :: Counter f
  , loop :: Counter f
  , nextAction :: f Sig
  , measure :: f Sig
  , trackIndex :: f Sig
  , partIndex :: f Sig
  , numOfParts :: f Sig
  }

{-| Column of clips. All clips in single column can not be played at the same time
so when next clip on given column starts previous one stops.
-}
newtype Column a = Column (Arr Sig a)

copyClipTo :: Update f -> ClipSt f -> ClipSt f -> SE ()
copyClipTo update from to =
  mapM_
    copy
    [ (.track)
    , (.bpm)
    , (.startTime)
    , (.timeSize)
    , (.change.size)
    , (.change.step)
    , (.loop.size)
    , (.loop.step)
    , (.nextAction)
    , (.measure)
    , (.trackIndex)
    , (.partIndex)
    , (.numOfParts)
    ]
  where
    copy f = update.write (f to) =<< update.read (f from)

data Counter f = Counter
  { size :: f Sig
  , step :: f Sig
  }

initCounter :: (Sig -> SE (f Sig)) -> Sig -> SE (Counter f)
initCounter cons maxSize =
  Counter
    <$> cons maxSize
    <*> cons maxSize

resetCounter :: Counter Ref -> Sig -> SE ()
resetCounter counter size = do
  writeRef counter.size size
  writeRef counter.step size

data Update f = Update
  { read :: f Sig -> SE Sig
  , write :: f Sig -> Sig -> SE ()
  , modify :: f Sig -> (Sig -> Sig) -> SE ()
  }

refUpdate :: Update Ref
refUpdate =
  Update
    { read = readRef
    , write = writeRef
    , modify = modifyRef
    }

columnUpdate :: ColumnId -> Update Column
columnUpdate columnId =
  Update
    { read = \(Column arr) -> readArr arr columnId
    , write = \(Column arr) value -> writeArr arr columnId value
    , modify = \(Column arr) f -> modifyArr arr columnId f
    }

nextCount :: Update f -> Counter f -> SE BoolSig
nextCount update counter = do
  update.modify counter.step (\x -> x - 1)
  current <- update.read counter.step
  when1 (current <=* 0) $
    update.write counter.step =<< update.read counter.size
  pure (current ==* 0)

initSt :: GetNextPart -> ExtraClipSize -> SE St
initSt getNextPart extraColumnSize = do
  bpm <- Bpm <$> newCtrlRef 120
  current <- initClipRef
  next <- initClipRef
  isTick <- newCtrlRef 0
  beatCounter <- initCounter newCtrlRef 1
  isMainClipChange <- newCtrlRef 0
  extraClips <-
    if extraColumnSize == 0
      then pure Nothing
      else Just <$> initExtraClips extraColumnSize
  main <- newProc (\() -> mainInstr getNextPart bpm beatCounter isTick isMainClipChange current next extraClips)
  pure St{..}

readClipSt :: St -> SE Clip
readClipSt st = do
  bpm <- readRef st.current.bpm
  start <- readRef st.current.startTime
  changeRate <- readRef st.current.change.size
  beatSize <- readRef st.current.loop.size
  timeSize <- readRef st.current.timeSize
  measure <- readRef st.current.measure
  nextAction <- readRef st.current.nextAction
  trackIndex <- readRef st.current.trackIndex
  partIndex <- readRef st.current.partIndex
  numOfParts <- readRef st.current.numOfParts
  pure Clip{..}

initClipRef :: SE ClipRef
initClipRef = initClipSt newCtrlRef

initClipSt :: (Sig -> SE (f Sig)) -> SE (ClipSt f)
initClipSt cons = do
  track <- cons (-1)
  bpm <- cons 120
  startTime <- cons 0
  timeSize <- cons 0.5
  change <- initCounter cons 1
  loop <- initCounter cons 1
  nextAction <- cons (toSig playLoop)
  measure <- cons 1
  trackIndex <- cons 0
  partIndex <- cons 0
  numOfParts <- cons 1
  pure ClipSt{..}

type GetNextPart = SE Part

getIsMainBeat :: Counter Ref -> SE BoolSig
getIsMainBeat ref = do
  step <- readRef ref.step
  size <- readRef ref.size
  pure (size ==* step)

mainInstr ::
  GetNextPart ->
  Bpm ->
  Counter Ref ->
  Ref Sig ->
  Ref Sig ->
  ClipRef ->
  ClipRef ->
  Maybe ExtraClips ->
  SE ()
mainInstr getNextPart bpm beatCounter isTickRef isMainClipChange current next extraClips =
  periodic bpm isTickRef $ do
    isMainBeat <- getIsMainBeat beatCounter
    void $ nextCount refUpdate beatCounter
    updateMainClip getNextPart bpm beatCounter isMainClipChange current next
    mapM_ (updateExtraClips bpm isMainBeat) extraClips

updateMainClip ::
  GetNextPart ->
  Bpm ->
  Counter Ref ->
  Ref Sig ->
  ClipRef ->
  ClipRef ->
  SE ()
updateMainClip getNextPart bpm beatCounter isMainClipChange current next = do
  isBoundary <- updateCounters refUpdate current
  onChanges getNextPart isBoundary bpm beatCounter isMainClipChange current next

updateExtraClips :: Bpm -> BoolSig -> ExtraClips -> SE ()
updateExtraClips bpm isMainBeat clips =
  foreachExtraClip clips $ \update -> do
    isBoundary <- updateCounters update clips.currents
    onExtraClipChanges update isBoundary isMainBeat bpm clips.currents clips.nexts

onExtraClipChanges :: Update Column -> IsBoundary -> BoolSig -> Bpm -> ClipColumn -> ClipColumn -> SE ()
onExtraClipChanges update boundary isMainBeat bpm currents nexts = do
  isClip <- isClipChange update currents nexts
  whens
    [ (boundary.isChange &&* isClip, onExtraClipChange update isMainBeat bpm currents nexts)
    , (boundary.isLoop, onExtraClipLoop update bpm currents nexts)
    ]
    (pure ())

onExtraClipChange :: Update Column -> BoolSig -> Bpm -> ClipColumn -> ClipColumn -> SE ()
onExtraClipChange update isMainBeat bpm currents nexts = do
  currentTrackId <- readTrackId update currents
  nextTrackId <- readTrackId update nexts
  let
    isPlaying = currentTrackId.track >* 0

  when1 (notB (equalTrackId currentTrackId nextTrackId) &&* (isPlaying ||* isMainBeat)) $ do
    stopClip update currents
    scheduleExtraClip bpm nextTrackId
    copyClipTo update nexts currents

scheduleExtraClip :: Bpm -> TrackId -> SE ()
scheduleExtraClip (Bpm bpmRef) (TrackId track start dur bpm _measure) = do
  mainBpm <- readRef bpmRef
  when1 (track >* 0) $ play (instrRefFromNum $ toD track) [Note 0 (toD $ dur * bpm / mainBpm) (toD mainBpm, toD start)]

foreachExtraClip :: ExtraClips -> (Update Column -> SE ()) -> SE ()
foreachExtraClip clips act = do
  iter <- newLocalCtrlRef (0 :: Sig)
  whileDo iter (\x -> x <* int clips.size) $ do
    index <- readRef iter
    act (columnUpdate index)
    writeRef iter (index + 1)

onExtraClipLoop :: Update Column -> Bpm -> ClipColumn -> ClipColumn -> SE ()
onExtraClipLoop update bpm currents nexts = do
  nextAction <- update.read currents.nextAction
  whens
    [ (isPlayLoop nextAction, onExtraClipPlayLoop update bpm currents)
    , (isPlayNext nextAction, pure ()) -- next action is not supported by extra clips
    , (isStopPlayback nextAction, onStopPlayback update currents nexts)
    ]
    (pure ())

onExtraClipPlayLoop :: Update Column -> Bpm -> ClipColumn -> SE ()
onExtraClipPlayLoop update (Bpm bpmRef) current = do
  mainBpm <- readRef bpmRef
  trackId <- readTrackId update current
  when1 (trackId.track >* 0) $
    play (instrRefFromNum $ toD trackId.track) [Note 0 (toD $ trackId.dur * trackId.bpm / mainBpm) (toD mainBpm, toD trackId.start)]

onChanges :: GetNextPart -> IsBoundary -> Bpm -> Counter Ref -> Ref Sig -> ClipRef -> ClipRef -> SE ()
onChanges getNextPart boundary bpm beatCounter isMainClipChange current next = do
  isClip <- isClipChange refUpdate current next
  whens
    [
      ( (boundary.isChange ||* boundary.isLoop) &&* isClip
      , do
          onChange bpm beatCounter current next
          writeRef isMainClipChange 1
      )
    , (boundary.isLoop, onLoop getNextPart bpm isMainClipChange current next)
    ]
    (writeRef isMainClipChange 0)

isClipChange :: Update f -> ClipSt f -> ClipSt f -> SE BoolSig
isClipChange update current next = do
  currentTrackId <- readTrackId update current
  nextTrackId <- readTrackId update next
  pure (notB $ equalTrackId currentTrackId nextTrackId)

onChange :: Bpm -> Counter Ref -> ClipRef -> ClipRef -> SE ()
onChange bpm beatCounter current next = do
  nextTrackId <- readTrackId refUpdate next
  stopClip refUpdate current
  copyClipTo refUpdate next current
  resetCounter beatCounter nextTrackId.measure
  scheduleClip bpm next

stopClip :: Update f -> ClipSt f -> SE ()
stopClip update clip = do
  track <- update.read clip.track
  when1 (track >* 0) $
    turnoff2 (instrRefFromNum @D $ toD track) 0 0.05

scheduleClip :: Bpm -> ClipRef -> SE ()
scheduleClip (Bpm bpmRef) clip = do
  writeRef bpmRef =<< readRef clip.bpm
  TrackId track start dur _bpm _measure <- readTrackId refUpdate clip
  when1 (track >* 0) $ eventD "i" (toD track) 0 dur start

data TrackId = TrackId
  { track :: Sig
  , start :: Sig
  , dur :: Sig
  , bpm :: Sig
  , measure :: Sig
  }

equalTrackId :: TrackId -> TrackId -> BoolSig
equalTrackId (TrackId a1 b1 c1 d1 e1) (TrackId a2 b2 c2 d2 e2) =
  a1 ==* a2 &&* b1 ==* b2 &&* c1 ==* c2 &&* d1 ==* d2 &&* e1 ==* e2

readTrackId :: Update f -> ClipSt f -> SE TrackId
readTrackId update clip =
  TrackId
    <$> update.read clip.track
    <*> update.read clip.startTime
    <*> update.read clip.timeSize
    <*> update.read clip.bpm
    <*> update.read clip.measure

onLoop :: GetNextPart -> Bpm -> Ref Sig -> ClipRef -> ClipRef -> SE ()
onLoop getNextPart bpm isMainClipChange current next = do
  nextAction <- readRef current.nextAction
  whens
    [ (isPlayLoop nextAction, onPlayLoop current)
    ,
      ( isPlayNext nextAction
      , do
          onPlayNext getNextPart bpm current next
          writeRef isMainClipChange 1
      )
    , (isStopPlayback nextAction, onStopPlayback refUpdate current next)
    ]
    (writeRef isMainClipChange 0)

isPlayLoop :: Sig -> BoolSig
isPlayLoop x = x ==* toSig playLoop

onPlayLoop :: ClipRef -> SE ()
onPlayLoop current = do
  track <- readRef current.track
  startTime <- readRef current.startTime
  dur <- readRef current.timeSize
  play (instrRefFromNum $ toD track) [Note 0 (toD dur) (toD startTime)]

isPlayNext :: Sig -> BoolSig
isPlayNext x = x ==* toSig playNext

onPlayNext :: GetNextPart -> Bpm -> ClipRef -> ClipRef -> SE ()
onPlayNext getNextPart bpm current next = do
  part <- getNextPart
  setNextPart writeRef next part
  stopClip refUpdate current
  scheduleClip bpm next
  copyClipTo refUpdate next current

isStopPlayback :: Sig -> BoolSig
isStopPlayback x = x ==* toSig stopPlayback

onStopPlayback :: Update f -> ClipSt f -> ClipSt f -> SE ()
onStopPlayback update current next = do
  update.write current.track 0
  update.write next.track 0

data IsBoundary = IsBoundary
  { isChange :: BoolSig
  , isLoop :: BoolSig
  }

updateCounters :: Update f -> ClipSt f -> SE IsBoundary
updateCounters update clip = do
  isChange <- nextCount update clip.change
  isLoop <- nextCount update clip.loop
  pure IsBoundary{..}

periodic :: Bpm -> Ref Sig -> SE () -> SE ()
periodic (Bpm bpmRef) tickRef onTick = do
  bpm <- readRef bpmRef
  writeRef tickRef (metro (toAbsTimeRate bpm 1))
  isTick <- readRef tickRef
  when1 (isTick ==* 1) onTick

setTrackPartSt :: St -> Part -> SE ()
setTrackPartSt st part = do
  setNextPart writeRef st.next part

setClipPartSt :: St -> ColumnId -> Part -> SE ()
setClipPartSt st columnId part = do
  mapM_ (\clips -> setOnColumn clips.size clips.nexts) st.extraClips
  where
    setOnColumn :: ExtraClipSize -> ClipColumn -> SE ()
    setOnColumn size nexts = do
      when1 (columnId <* int size) $
        setNextPart (writeColumn columnId) nexts part

writeColumn :: (Tuple a) => ColumnId -> Column a -> a -> SE ()
writeColumn columnId (Column arr) value =
  writeArr arr columnId value

setNextPart :: (f Sig -> Sig -> SE ()) -> ClipSt f -> Part -> SE ()
setNextPart writeTo next part = do
  write (.track) part.track
  write (.bpm) part.clip.bpm
  write (.startTime) part.clip.start
  write (.timeSize) part.clip.timeSize
  write (.change.size) part.clip.changeRate
  write (.change.step) part.clip.changeRate
  write (.loop.size) part.clip.beatSize
  write (.loop.step) part.clip.beatSize
  write (.nextAction) part.clip.nextAction
  write (.measure) part.clip.measure
  write (.trackIndex) part.clip.trackIndex
  write (.partIndex) part.clip.partIndex
  write (.numOfParts) part.clip.numOfParts
  where
    write f = writeTo (f next)

startSt :: St -> SE ()
startSt st =
  play st.main [Note 0 (-1) ()]

stopSt :: St -> SE ()
stopSt st =
  turnoff2_i st.main 0 0.05

-- | Converts beats to seconds
toAbsTimeRate :: (SigOrD a) => a -> a -> a
toAbsTimeRate bpm beats = bpm / (60 * beats)

readBpmSt :: St -> SE Sig
readBpmSt st =
  case st.bpm of
    Bpm ref -> readRef ref

readTicksSt :: St -> SE Sig
readTicksSt st =
  readRef st.isTick

currentBeatSt :: St -> SE Sig
currentBeatSt st = do
  step <- readRef st.beatCounter.step
  size <- readRef st.beatCounter.size
  pure (size - step)
