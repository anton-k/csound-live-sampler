-- | Playlist keeps information on track and clip order.
-- We can query clip info for the given track. And navigate through
-- parts.
module Live.Scene.Playlist
  ( Playlist (..)
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

import Csound.Core

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

newPlaylist :: TrackConfig -> SE Playlist
newPlaylist = undefined
