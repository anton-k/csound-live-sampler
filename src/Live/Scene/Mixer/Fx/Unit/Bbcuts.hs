module Live.Scene.Mixer.Fx.Unit.Bbcuts
  ( bbcutUnit
  ) where

import Live.Scene.Mixer.Fx.Unit
import Live.Scene.Mixer.Fx.Config (BbcutConfig (..))
import Csound.Core

bbcutUnit :: Unit BbcutConfig
bbcutUnit =
  Unit
    { needsBpm = True
    , getParams = bbcutParams
    , apply = bbcutFx
    }

bbcutParams :: BbcutConfig -> SE ParamMap
bbcutParams config =
  newParamMap config [("dryWet", (.dryWet))]

bbcutFx :: Bpm -> ParamMap -> BbcutConfig -> Sig2 -> SE Sig2
bbcutFx (Bpm readBpm) params config ain = do
  bpm <- toD . ir <$> readBpm
  dryWet <- readParam params "dryWet"
  pure $ mixAt dryWet (\(aleft, aright) ->
    bbcuts aleft aright (bpm / 60)
      (float config.subdiv)
      (float config.barlength)
      (float config.phrasebars)
      (float config.numrepeats)
    ) ain
