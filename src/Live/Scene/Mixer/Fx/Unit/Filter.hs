module Live.Scene.Mixer.Fx.Unit.Filter
  ( moogUnit
  , korgUnit
  , cutoffParam
  ) where

import Live.Scene.Mixer.Fx.Unit
import Live.Scene.Mixer.Fx.Config (MoogConfig, KorgConfig, ResonantFilterConfig (..))
import Csound.Core
import Data.Maybe

moogUnit :: Unit MoogConfig
moogUnit =
  resonantFilterUnit (\cutoff resonance input -> moogvcf2 input cutoff resonance)

korgUnit :: Unit KorgConfig
korgUnit =
  resonantFilterUnit (\cutoff resonance input -> k35_lpf input cutoff resonance)

type FilterFun = Sig -> Sig -> Sig -> Sig

resonantFilterUnit :: FilterFun -> Unit ResonantFilterConfig
resonantFilterUnit f =
  Unit
    { needsBpm = False

    , getParams = \config ->
        newParamMap config
          [ ("cutoff", (.cutoff))
          , ("resonance", (.resonance))
          , ("dryWet", fromMaybe 1 . (.dryWet))
          ]

    , apply = \_bpm params _config -> resonFx f params
    }

resonFx :: FilterFun -> ParamMap -> Sig2 -> SE Sig2
resonFx applyFilter params ins = do
  dryWet <- param "dryWet"
  cutoff <- cutoffParam <$> param "cutoff"
  resonance <- param "resonance"
  pure $ mixAt dryWet (applyFilter cutoff resonance) ins
  where
    param = readParam params

cutoffFrequencyRange :: Num a => (a, a)
cutoffFrequencyRange = (10, 20000)

cutoffParam :: Sig -> Sig
cutoffParam = frequencyParam cutoffFrequencyRange
