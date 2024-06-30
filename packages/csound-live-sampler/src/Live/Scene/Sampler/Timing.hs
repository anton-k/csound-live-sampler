-- | Module to slice stems by cues
module Live.Scene.Sampler.Timing (
  Clip (..),
  splitStem,
) where

import Data.List qualified as List
import Data.Maybe
import Live.Scene.Sampler.Config

data Clip = Clip
  { bpm :: Float
  , start :: Float
  , changeRate :: Int
  , beatSize :: Int
  , timeSize :: Float
  , nextAction :: NextAction
  , measure :: Int
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
    initSt =
      St
        { bpm_ = 120
        , measure = (4, 4)
        , lastTime = 0
        , changeRate = 4
        , clips = []
        }

    go :: St -> TimeSlot -> St
    go st slot =
      List.foldl' (splitSlot timeScale measure) (enterSlot slot st) slot.cues
      where
        timeScale = fromMaybe 1 slot.timeScale
        measure = fromMaybe (4, 4) slot.measure

enterSlot :: TimeSlot -> St -> St
enterSlot slot st =
  st
    { bpm_ = slot.bpm
    , measure = fromMaybe st.measure slot.measure
    , changeRate = timeScale * fromMaybe st.changeRate slot.changeRate
    }
  where
    timeScale = fromMaybe 1 slot.timeScale

splitSlot :: Int -> (Int, Int) -> St -> Cue -> St
splitSlot timeScale measure st cue =
  st
    { lastTime = st.lastTime + timeSize
    , clips = clip : st.clips
    }
  where
    start = fromMaybe 0 cue.start

    beatSize = start + cue.dur

    timeSize = toAbsTime st (fromIntegral timeScale * beatSize)

    clip =
      Clip
        { bpm = st.bpm_
        , start = st.lastTime + toAbsTime st (fromIntegral timeScale * start)
        , changeRate = timeScale * st.changeRate
        , beatSize = round (fromIntegral timeScale * cue.dur)
        , timeSize = toAbsTime st (fromIntegral timeScale * cue.dur)
        , nextAction = fromMaybe PlayLoop cue.nextAction
        , measure = fst measure
        }

-- | Converts beats to seconds
toAbsTime :: St -> Float -> Float
toAbsTime st beats = 60 * beats / st.bpm_
