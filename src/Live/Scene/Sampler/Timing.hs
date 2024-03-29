-- | Module to slice stems by cues
module Live.Scene.Sampler.Timing
  ( Clip (..)
  , splitStem
  ) where

import Data.Maybe
import Live.Scene.Sampler.Config
import Data.List qualified as List

data Clip = Clip
  { bpm :: Float
  , start :: Float
  , changeRate :: Int
  , beatSize :: Int
  , timeSize :: Float
  }
  deriving (Eq, Show)

data St = St
  { bpm_ :: Float
  , measure :: (Int, Int)
  , lastTime :: Float
  , changeRate :: Int
  , clips :: [Clip]
  }

splitStem :: [TimeSlot] -> [Clip]
splitStem slots =
  List.reverse . (.clips) $ List.foldl' go initSt slots
  where
    initSt = St
      { bpm_ = 120
      , measure = (4, 4)
      , lastTime = 0
      , changeRate = 4
      , clips = []
      }

    go :: St -> TimeSlot -> St
    go st slot =
      List.foldl' splitSlot (enterSlot slot st) slot.cues

enterSlot :: TimeSlot -> St -> St
enterSlot slot st =
  st
    { bpm_ = slot.bpm
    , measure = fromMaybe st.measure slot.measure
    , changeRate = fromMaybe st.changeRate slot.changeRate
    }

splitSlot :: St -> Cue -> St
splitSlot st cue =
  st
    { lastTime = st.lastTime + timeSize
    , clips = clip : st.clips
    }
  where
    start = fromMaybe 0 cue.start

    beatSize = start + cue.dur

    timeSize = toAbsTime st (fromIntegral beatSize)

    clip = Clip
      { bpm = st.bpm_
      , start = st.lastTime + toAbsTime st (fromIntegral start)
      , changeRate = st.changeRate
      , beatSize = cue.dur
      , timeSize = toAbsTime st (fromIntegral cue.dur)
      }

-- | Converts beats to seconds
toAbsTime :: St -> Float -> Float
toAbsTime st beats = 60 * beats / st.bpm_
