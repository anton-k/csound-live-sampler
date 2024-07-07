module Live.Scene.Osc (
  setupOsc,
  OscConfigs (..),
) where

import Csound.Core
import Live.Scene.Osc.Config
import Live.Scene.Osc.Input
import Live.Scene.Osc.Output
import Live.Scene.Osc.Ui (getUiOscMessage)
import Live.Scene.Types

setupOsc :: OscConfigs -> Scene -> SE ()
setupOsc config scene = do
  mapM_ (setupOscInput scene config inputDep) config.osc.input
  mapM_ (setupOscOutput scene) config.osc.output
  where
    inputDep =
      OscInputDep
        { sendUiInfo = \isSend address ->
            case config.osc.output of
              Just outputConfig -> send outputConfig (ifB isSend 1 0) address uiInfo
              Nothing -> pure ()
        }

    uiInfo :: Str
    uiInfo = getUiOscMessage config.mixer config.sampler
