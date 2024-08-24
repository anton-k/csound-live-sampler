module Live.Scene.Osc (
  setupOsc,
  OscConfigs (..),
) where

import Csound.Core
import Data.Default
import Data.Maybe
import Live.Scene.Common (ChannelId (..))
import Live.Scene.Osc.Config
import Live.Scene.Osc.Input
import Live.Scene.Osc.Output
import Live.Scene.Osc.Ui (getUiOscMessage)
import Live.Scene.Types

setupOsc :: OscConfigs -> Scene -> SE ()
setupOsc config scene = do
  mapM_ (setupOscInput scene config inputDep) config.osc.input
  mapM_ (setupOscOutput scene config) config.osc.output
  where
    inputDep =
      OscInputDep
        { sendUiInfo = \isSend address -> withOutput $ \outputConfig ->
            send outputConfig (ifB isSend 1 0) address uiInfo
        , sendCurrentPart = \isSend -> withOutput $ \outputConfig ->
            sendCurrentSamplerPart outputConfig scene.sampler (ifB isSend 1 0)
        }

    uiInfo :: Str
    uiInfo = getUiOscMessage config.mixer config.sampler (fromMaybe def config.osc.ui) config.card

    withOutput :: (OscOutputConfig ChannelId -> SE ()) -> SE ()
    withOutput cont =
      case config.osc.output of
        Just outputConfig -> cont outputConfig
        Nothing -> pure ()
