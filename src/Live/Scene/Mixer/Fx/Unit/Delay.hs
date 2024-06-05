module Live.Scene.Mixer.Fx.Unit.Delay
  ( delayUnit
  , pingPongUnit
  , toDelayTime
  ) where

import Live.Scene.Mixer.Fx.Unit
import Live.Scene.Mixer.Fx.Config (DelayConfig (..), PingPongConfig (..))
import Csound.Core
import Csound.Core.Opcodes.Fx

delayUnit :: Unit DelayConfig
delayUnit = Unit
  { needsBpm = True
  , getParams = delayParams
  , apply = delayFx
  }

delayParams :: DelayConfig -> SE ParamMap
delayParams config =
  newParamMap config
    [ ("damp", (.damp))
    , ("feedback", (.feedback))
    , ("dryWet", (.dryWet))
    ]

delayFx :: Bpm -> ParamMap -> DelayConfig -> Sig2 -> SE Sig2
delayFx (Bpm readBpm) params config ins = do
  bpm <- toD . ir <$> readBpm
  dryWet <- param "dryWet"
  feedback <- param "feedback"
  damp <- param "damp"
  let
    time = toSig (toDelayTime bpm (float config.repeatTime))
  pure $ at (\x -> analogDelay x dryWet time feedback damp) ins
  where
    param = readParam params

pingPongUnit :: Unit PingPongConfig
pingPongUnit = Unit
  { needsBpm = True
  , getParams = pingPongParams
  , apply = pingPongFx
  }

pingPongParams :: PingPongConfig -> SE ParamMap
pingPongParams config =
  newParamMap config
    [ ("damp", (.damp))
    , ("feedback", (.feedback))
    , ("width", (.width))
    , ("dryWet", (.dryWet))
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
  pure $ pingPongDelay ins (toSig time) feedback dryWet width damp (time * 2)
  where
    param = readParam params

-- * utils

toDelayTime :: D -> D -> D
toDelayTime bpm beats = 4 * beats * 60 / bpm
