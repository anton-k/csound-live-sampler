-- | Engine can play instruments in the loop syncronized on BPM.
--
-- Note that engine can play any instrument which has single
-- argument: skip start time
module Live.Scene.Sampler.Engine
  ( Part (..)
  , StartTime
  , ClipInstr
  , Engine (..)
  , newEngine
  ) where

import Csound.Core
import Live.Scene.Sampler.Timing (Clip (..))

type StartTime = D
type ClipInstr = InstrRef StartTime

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
  , prev :: ClipSt
  , next :: ClipSt
  }

data Bpm = Bpm
  { ref :: Ref Sig
  }

data ClipSt = ClipSt
  { track :: Ref Sig
  , startTime :: Ref Sig
  , changeRate :: Ref Sig
  , beatSize :: Ref Sig
  , timeSize :: Ref Sig
  , step :: Ref Sig
  }

initSt :: SE St
initSt = do
  bpm <- initBpm
  prev <- initClipSt
  next <- initClipSt
  main <- newProc (\() -> mainInstr bpm prev next)
  pure St {..}
  where
    initBpm = Bpm <$> newRef 120

    initClipSt = do
      track <- newCtrlRef (-1)
      startTime <- newCtrlRef 0
      changeRate <- newCtrlRef 1
      beatSize <- newCtrlRef 1
      timeSize <- newCtrlRef 0.5
      step <- newCtrlRef 1
      pure ClipSt {..}

mainInstr :: Bpm -> ClipSt -> ClipSt -> SE ()
mainInstr bpm prev next = undefined

setPartSt :: St -> Part -> SE ()
setPartSt = undefined

startSt :: St -> SE ()
startSt st =
  play st.main [Note 0 (-1) ()]

stopSt :: St -> SE ()
stopSt st =
  turnoff2_i st.main 0 0.05
