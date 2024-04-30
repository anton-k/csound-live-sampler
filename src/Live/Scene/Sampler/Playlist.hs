-- | Playlist keeps information on track and clip order.
-- We can query clip info for the given track. And navigate through
-- parts.
module Live.Scene.Sampler.Playlist
  ( Playlist (..)
  , Cursor (..)
  , Part (..)
  , ClipInstr
  , Clip (..)
  , newPlaylist
  , TrackId (..)
  , ClipId (..)
  , nextTrack
  , prevTrack
  , nextPart
  , prevPart
  , setTrack
  , setPart
  ) where

import Prelude hiding ((<*))
import Data.Boolean
import Csound.Core
import Live.Scene.Sampler.Timing qualified as Timing
import Live.Scene.Sampler.Config
import Live.Scene.Sampler.Engine (Part (..), ClipInstr, Clip (..))

newtype TrackId = TrackId D
  deriving newtype (Tuple, Arg)

newtype ClipId = ClipId D
  deriving newtype (Tuple, Arg)

data Playlist = Playlist
  { getPart :: SE Part
  , cursor :: Cursor
  }

data Cursor = Cursor
  { modifyTrack :: (Sig -> Sig) -> SE ()
  , modifyPart :: (Sig -> Sig) -> SE ()
  }

nextTrack :: Cursor -> SE ()
nextTrack cursor = cursor.modifyTrack (+1)

prevTrack :: Cursor -> SE ()
prevTrack cursor = cursor.modifyTrack (\x -> x - 1)

nextPart :: Cursor -> SE ()
nextPart cursor = cursor.modifyPart (+1)

prevPart :: Cursor -> SE ()
prevPart cursor = cursor.modifyPart (\x -> x - 1)

setTrack :: Cursor -> TrackId -> SE ()
setTrack cursor (TrackId trackId) =
  cursor.modifyTrack (const $ toSig trackId)

setPart :: Cursor -> TrackId -> ClipId -> SE ()
setPart cursor (TrackId trackId) (ClipId clipId) = do
  cursor.modifyTrack (const $ toSig trackId)
  cursor.modifyPart (+ toSig clipId)

newPlaylist :: SamplerConfig -> [ClipInstr] -> SE Playlist
newPlaylist config instrs = do
  st <- initSt config instrs
  pure $
    Playlist
      { getPart = getPartSt st
      , cursor = initCursor st
      }

type PartArray = Arr Sig Part
type TrackStartArray = Arr Sig Sig
type ClipToTrackArray = Arr Sig Sig

data St = St
  { infos :: PartArray
  , trackStarts :: TrackStartArray
  , clipToTrack :: ClipToTrackArray
  , index :: Ref Sig
  }

getPartSt :: St -> SE Part
getPartSt st = do
  index <- readRef st.index
  readArr st.infos index

initCursor :: St -> Cursor
initCursor st =
  Cursor
    { modifyTrack = modifyTrackSt st
    , modifyPart = modifyPartSt st
    }

modifyTrackSt :: St -> (Sig -> Sig) -> SE ()
modifyTrackSt st f = do
  index <- readRef st.index
  trackId <- readArr st.clipToTrack index
  -- trackStart <- readArr st.trackStarts trackId
  let
    nextTrackId = wrapArrayBounds st.trackStarts (f trackId)
    {- diff = index - trackStart -}
  nextTrackStart <- readArr st.trackStarts nextTrackId
  writeRef st.index $ wrapArrayBounds st.infos $ nextTrackStart {- + diff -}

modifyPartSt :: St -> (Sig -> Sig) -> SE ()
modifyPartSt st f = do
  modifyRef st.index (wrapArrayBounds st.infos .  f)

-- | TODO: make wrap strategy configurtable: also consider loop
wrapArrayBounds :: Arr Sig a -> Sig -> Sig
wrapArrayBounds arr index =
  ifB (index <* 0)
    0
    (ifB (index >=* len)
      (len - 1)
      index)
  where
    len = toSig (lenarray arr)

initSt :: SamplerConfig -> [ClipInstr] -> SE St
initSt config instrs = do
  infos <- initPartArray tracks
  trackStarts <- initTrackStartArray tracks
  clipToTrack <- initClipToTrackArray tracks
  index <- newCtrlRef 0
  pure St {..}
  where
    tracks = timeTracks config instrs

initPartArray :: [TimedTrack] -> SE PartArray
initPartArray tracks =
  fillGlobalCtrlArr [length parts] parts
  where
    parts = toPart <$> ((\track -> fmap (\clip -> (track.instr, clip)) track.clips) =<< tracks)

    toPart :: (ClipInstr, Timing.Clip) -> Part
    toPart (instr, clip) =
      Part
        { track = instr
        , clip =
            Clip
              { bpm = float clip.bpm
              , start = float clip.start
              , changeRate = int clip.changeRate
              , beatSize = int clip.beatSize
              , timeSize = float clip.timeSize
              , nextAction = int $ fromEnum clip.nextAction
              }
        }

initTrackStartArray :: [TimedTrack] -> SE TrackStartArray
initTrackStartArray tracks =
  fillGlobalCtrlArr
    [length tracks]
    (fmap (toSig . int) $ init $ scanl (+) 0 $ fmap (length . (.clips)) tracks)

initClipToTrackArray :: [TimedTrack] -> SE ClipToTrackArray
initClipToTrackArray tracks =
  fillGlobalCtrlArr [length ids] ids
  where
    ids = concat $ zipWith clipToTrackIds [0..] tracks

    clipToTrackIds :: Int -> TimedTrack -> [Sig]
    clipToTrackIds trackId track =
      fmap (const $ int trackId) track.clips

data TimedTrack = TimedTrack
  { track :: TrackConfig
  , clips :: [Timing.Clip]
  , instr :: ClipInstr
  }

timeTracks :: SamplerConfig -> [ClipInstr] -> [TimedTrack]
timeTracks config instrs =
  zipWith toTimedTrack instrs config.tracks

toTimedTrack :: ClipInstr -> TrackConfig -> TimedTrack
toTimedTrack instr config =
  TimedTrack
    { track = config
    , clips = Timing.splitStem config.slots
    , instr
    }
