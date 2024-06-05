module Live.Scene.Mixer.Fx.Unit.Reverb
  ( reverbUnit
  ) where

import Live.Scene.Mixer.Fx.Unit
import Live.Scene.Mixer.Fx.Config (ReverbConfig (..))
import Csound.Core

reverbUnit :: Unit ReverbConfig
reverbUnit = Unit
  { needsBpm = False
  , getParams = reverbParams
  , apply = \_bpm params _config -> reverbFx params
  }

reverbParams :: ReverbConfig -> SE ParamMap
reverbParams config =
  newParamMap config [("size", (.size)), ("damp", (.damp)), ("dryWet", (.dryWet))]

reverbFx :: ParamMap -> Sig2 -> SE Sig2
reverbFx params ins = do
  dryWet <- param "dryWet"
  size <- param "size"
  damp <- dampParam <$> param "damp"
  pure $ mixAt dryWet (reverbsc size damp) ins
  where
    param = readParam params
