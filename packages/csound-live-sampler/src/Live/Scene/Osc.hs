module Live.Scene.Osc (
  setupOsc,
  OscConfigs (..),
) where

import Csound.Core
import Live.Scene.Osc.Config
import Live.Scene.Osc.Input
import Live.Scene.Osc.Output
import Live.Scene.Types

setupOsc :: OscConfigs -> Scene -> SE ()
setupOsc config scene = do
  mapM_ (setupOscInput scene config) config.osc.input
  mapM_ (setupOscOutput scene) config.osc.output
