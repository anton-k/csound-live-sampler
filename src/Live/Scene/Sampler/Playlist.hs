-- | Playlist keeps information on track and clip order.
-- We can query clip info for the given track. And navigate through
-- parts.
module Live.Scene.Sampler.Playlist
  ( Playlist (..)
  , Cursor (..)
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
import Control.Monad
import Data.Boolean
import Csound.Core
import Live.Scene.Sampler.Timing qualified as Timing
import Live.Scene.Sampler.Config
import Live.Scene.Sampler.Engine (Part (..), ClipInstr, Clip (..))

data TrackId = TrackId D
data ClipId = ClipId D

data Playlist = Playlist
  { getPart :: SE Part
  , cursor :: Cursor
  }

data Cursor = Cursor
  { modifyTrack :: (D -> D) -> SE ()
  , modifyPart :: (D -> D) -> SE ()
  }

nextTrack :: Playlist -> SE ()
nextTrack playlist = playlist.cursor.modifyTrack (+1)

prevTrack :: Playlist -> SE ()
prevTrack playlist = playlist.cursor.modifyTrack (\x -> x - 1)

nextPart :: Playlist -> SE ()
nextPart playlist = playlist.cursor.modifyPart (+1)

prevPart :: Playlist -> SE ()
prevPart playlist = playlist.cursor.modifyPart (\x -> x - 1)

setTrack :: Playlist -> TrackId -> SE ()
setTrack playlist (TrackId trackId) = playlist.cursor.modifyTrack (const trackId)

setPart :: Playlist -> TrackId -> ClipId -> SE ()
setPart playlist (TrackId trackId) (ClipId clipId) =
  playlist.cursor.modifyTrack ((+ clipId) . const trackId)

newPlaylist :: SamplerConfig -> [ClipInstr] -> SE Playlist
newPlaylist config instrs = do
  st <- initSt config instrs
  pure $
    Playlist
      { getPart = getPartSt st
      , cursor = initCursor st
      }

type PartArray = Arr D Part
type TrackStartArray = Arr D D
type ClipToTrackArray = Arr D D

data St = St
  { infos :: PartArray
  , trackStarts :: TrackStartArray
  , cliptoTrack :: ClipToTrackArray
  , index :: Ref Sig
  }

getPartSt :: St -> SE Part
getPartSt st = do
  index <- readInitRef st.index
  readArr st.infos index

initCursor :: St -> Cursor
initCursor st =
  Cursor
    { modifyTrack = modifyTrackSt st
    , modifyPart = modifyPartSt st
    }

modifyTrackSt :: St -> (D -> D) -> SE ()
modifyTrackSt st f = do
  index <- readInitRef st.index
  trackId <- readArr st.cliptoTrack index
  trackStart <- readArr st.trackStarts trackId
  let
    nextTrackId = wrapArrayBounds st.trackStarts (f trackId)
    diff = index - trackStart
  nextTrackStart <- readArr st.trackStarts nextTrackId
  writeInitRef st.index $ wrapArrayBounds st.infos $ nextTrackId + diff

modifyPartSt :: St -> (D -> D) -> SE ()
modifyPartSt st f = do
  modifyInitRef st.index (wrapArrayBounds st.infos .  f)

wrapArrayBounds :: Arr D a -> D -> D
wrapArrayBounds arr index =
  ifB (index <* 0)
    0
    (ifB (index >=* len)
      (len - 1)
      index)
  where
    len = lenarray arr

initSt :: SamplerConfig -> [ClipInstr] -> SE St
initSt config instrs = do
  infos <- initPartArray tracks
  trackStarts <- initTrackStartArray tracks
  cliptoTrack <- initClipToTrackArray tracks
  index <- newCtrlRef 0
  pure St {..}
  where
    tracks = timeTracks config instrs

initPartArray :: [TimedTrack] -> SE PartArray
initPartArray tracks =
  fillGlobalCtrlArr [length parts] parts
  where
    parts = toPart <$> ((\track -> fmap (\clip -> (track.track, track.instr, clip)) track.clips) =<< tracks)

    toPart :: (TrackConfig, ClipInstr, Timing.Clip) -> Part
    toPart (config, instr, clip) =
      Part
        { track = instr
        , clip =
            Clip
              { bpm = float clip.bpm
              , start = float clip.start
              , changeRate = int clip.changeRate
              , beatSize = int clip.beatSize
              , timeSize = float clip.timeSize
              }
        }

initTrackStartArray :: [TimedTrack] -> SE TrackStartArray
initTrackStartArray tracks =
  fillGlobalCtrlArr
    [length tracks]
    (fmap int $ init $ scanl (+) 0 $ fmap (length . (.clips)) tracks)

initClipToTrackArray :: [TimedTrack] -> SE ClipToTrackArray
initClipToTrackArray tracks =
  fillGlobalCtrlArr [length ids] ids
  where
    ids = concat $ zipWith clipToTrackIds [0..] tracks

    clipToTrackIds :: Int -> TimedTrack -> [D]
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
