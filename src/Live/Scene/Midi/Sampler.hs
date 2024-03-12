module Live.Scene.Midi.Sampler
  ( changeTrackInstr
  ) where

import Live.Scene.Sampler
import Live.Scene.Midi.Config
import Csound.Core
import Data.Boolean

changeTrackInstr :: TrackChangesMidiConfig -> Sampler -> SE BoolD -> D -> SE ()
changeTrackInstr (TrackChangesMidiConfig trackButtons) sampler readShift = \noteId -> do
  isShift <- readShift
  whens (zipWith (instrCase isShift noteId) trackButtons sampler.getTrackIds) (pure ())
  where
    instrCase isShift buttonMidi buttonTrack trackId = (condition, body)
      where
        condition = isShift &&* (buttonMidi ==* int buttonTrack)

        body = setTrack sampler.cursor trackId
