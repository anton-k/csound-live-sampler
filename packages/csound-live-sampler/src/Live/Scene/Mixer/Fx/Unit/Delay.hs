module Live.Scene.Mixer.Fx.Unit.Delay (
  delayUnit,
  pingPongUnit,
  toDelayTime,
) where

import Csound.Core
import Data.Map.Strict qualified as Map
import Live.Scene.Mixer.Fx.Config (DelayConfig (..), PingPongConfig (..))
import Live.Scene.Mixer.Fx.Unit

delayUnit :: Unit DelayConfig
delayUnit =
  Unit
    { needsBpm = True
    , getParams = delayParams
    , apply = delayFx
    }

delayParams :: DelayConfig -> ParamNameInitMap
delayParams DelayConfig{..} =
  Map.fromList
    [ ("damp", damp)
    , ("feedback", feedback)
    , ("dryWet", dryWet)
    ]

delayFx :: Bpm -> ParamMap -> DelayConfig -> Sig2 -> SE Sig2
delayFx (Bpm readBpm) params config ins = do
  bpm <- toD . ir <$> readBpm
  dryWet <- param "dryWet"
  feedback <- param "feedback"
  damp <- param "damp"
  let
    time = toDelayTime bpm (float config.repeatTime)
  pure $ at (\x -> analogDelay x dryWet (toSig time) feedback damp (1.1 * time)) ins
  where
    param = readParam params

pingPongUnit :: Unit PingPongConfig
pingPongUnit =
  Unit
    { needsBpm = True
    , getParams = pingPongParams
    , apply = pingPongFx
    }

pingPongParams :: PingPongConfig -> ParamNameInitMap
pingPongParams PingPongConfig{..} =
  Map.fromList
    [ ("damp", damp)
    , ("feedback", feedback)
    , ("width", width)
    , ("dryWet", dryWet)
    ]

pingPongFx :: Bpm -> ParamMap -> PingPongConfig -> Sig2 -> SE Sig2
pingPongFx (Bpm readBpm) params config ins = do
  bpm <- toD . ir <$> readBpm
  dryWet <- param "dryWet"
  feedback <- param "feedback"
  width <- param "width"
  damp <- param "damp"
  let
    time = toDelayTime bpm (float config.repeatTime)
  pure $ pingPongDelay ins (toSig time) feedback dryWet width damp (time * 1.1)
  where
    param = readParam params

-- * utils

toDelayTime :: D -> D -> D
toDelayTime bpm beats = 4 * beats * 60 / bpm
