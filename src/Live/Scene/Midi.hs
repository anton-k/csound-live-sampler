module Live.Scene.Midi
  ( setupMidi
  ) where

import Live.Config
import Live.Scene.Mixer
import Live.Scene.Sampler
import Csound.Core
import Live.Scene.Midi.Act

setupMidi :: Config -> Mixer -> Sampler -> SE ()
setupMidi config mixer sampler = do
  setMidiActions mixer sampler config.controllers
