module Live.Scene.Mixer.Fx.Unit.Reverb (
  reverbUnit,
) where

import Csound.Core
import Data.Map.Strict qualified as Map
import Live.Scene.Mixer.Fx.Config (ReverbConfig (..))
import Live.Scene.Mixer.Fx.Unit

reverbUnit :: Unit ReverbConfig
reverbUnit =
  Unit
    { needsBpm = False
    , getParams = reverbParams
    , getName = (.name)
    , apply = \_bpm params _config -> reverbFx params
    }

reverbParams :: ReverbConfig -> ParamNameInitMap
reverbParams ReverbConfig{..} =
  Map.fromList [("size", size), ("damp", damp), ("dryWet", dryWet)]

reverbFx :: ParamMap -> Sig2 -> SE Sig2
reverbFx params ins = do
  dryWet <- param "dryWet"
  size <- param "size"
  damp <- dampParam <$> param "damp"
  pure $ mixAt dryWet (reverbsc size damp) ins
  where
    param = readParam params
