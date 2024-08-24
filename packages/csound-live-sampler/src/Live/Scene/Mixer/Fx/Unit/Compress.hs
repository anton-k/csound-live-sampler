module Live.Scene.Mixer.Fx.Unit.Compress (
  limiterUnit,
) where

import Csound.Core
import Live.Scene.Mixer.Fx.Config (LimiterConfig (..))
import Live.Scene.Mixer.Fx.Unit

limiterUnit :: Unit LimiterConfig
limiterUnit =
  Unit
    { needsBpm = False
    , getParams = limiterParams
    , apply = \_bpm _param config -> limiterFx config
    }

limiterParams :: LimiterConfig -> ParamNameInitMap
limiterParams _config = mempty

limiterFx :: LimiterConfig -> Sig2 -> SE Sig2
limiterFx config ins =
  pure $
    mul (float config.maxVolume) $
      at (\ain -> compress ain ain kthresh kloknee khiknee kratio katt krel ilook) ins
  where
    kthresh = 0
    kloknee = 95
    khiknee = 95
    kratio = 100
    katt = 0.005
    krel = 0.005
    ilook = 0
