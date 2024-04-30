module Live.Scene.Sampler
  ( Sampler (..)
  , TrackId (..)
  , newSampler
  , setTrack
  , module X
  ) where

import Csound.Core
import Live.Scene.Gen as X
import Live.Scene.Sampler.Config as X
import Live.Scene.Sampler.Engine
import Live.Scene.Sampler.Playlist
  ( Cursor (..)
  , setTrack
  , Playlist (..)
  , newPlaylist
  , TrackId (..)
  , nextPart
  )
import Live.Scene.Sampler.Audio (setupAudio)

data Sampler = Sampler
  { audio :: Gen
  , cursor :: Cursor
  , start :: SE ()
  , stop :: SE ()
  , getTrackIds :: [TrackId]
  }

newSampler :: SamplerConfig -> SE Sampler
newSampler config = do
  (audio, instrIds) <- setupAudio config
  playlist <- newPlaylist config instrIds
  let
    getNextPart = do
      nextPart playlist.cursor
      getPart playlist
  engine <- newEngine getNextPart
  pure $ Sampler
    { audio
    , cursor = initSamplerCursor playlist engine
    , start = engine.start
    , stop = engine.stop
    , getTrackIds = allTrackIds config
    }

allTrackIds :: SamplerConfig -> [TrackId]
allTrackIds config =
  TrackId . int <$> [0 .. length config.tracks - 1]

initSamplerCursor :: Playlist -> Engine -> Cursor
initSamplerCursor playlist engine =
  Cursor
    { modifyTrack = \f -> do
        playlist.cursor.modifyTrack f
        part <- playlist.getPart
        engine.setPart part

    , modifyPart = \f -> do
        playlist.cursor.modifyPart f
        part <- playlist.getPart
        engine.setPart part
    }
