module Live.Scene.Mixer.Fx (
  FxName (..),
  FxParams (..),
  readParamMap,
  modifyFxParam,
  setFxParam,
  unitToFun,
  Bpm (..),
  toFxName,
  toFxParamMap,
  toFxParamNameInitMap,
  isBpmSensitive,
  newFxParams,
  fxUnitName,
) where

import Csound.Core
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Live.Scene.Mixer.Fx.Config
import Live.Scene.Mixer.Fx.Unit
import Live.Scene.Mixer.Fx.Unit.Bbcuts (bbcutUnit)
import Live.Scene.Mixer.Fx.Unit.Compress (limiterUnit)
import Live.Scene.Mixer.Fx.Unit.Delay (delayUnit, pingPongUnit)
import Live.Scene.Mixer.Fx.Unit.Eq (eqUnit, mixerEqUnit)
import Live.Scene.Mixer.Fx.Unit.Filter (korgUnit, moogUnit)
import Live.Scene.Mixer.Fx.Unit.Reverb (reverbUnit)
import Live.Scene.Mixer.Fx.Unit.Tool (toolUnit)
import Prelude hiding (read)

newtype FxName = FxName {text :: Text}
  deriving newtype (Eq, Ord, Show)

newtype FxParams = FxParams (Map FxName ParamMap)
  deriving newtype (Semigroup, Monoid)

modifyFxParam :: FxParams -> FxName -> FxParamName -> (Sig -> Sig) -> SE ()
modifyFxParam (FxParams nameMap) name param f = do
  mapM_ (\ref -> modifyRef ref f) mParam
  where
    mParam = do
      paramMap <- Map.lookup name nameMap
      Map.lookup param paramMap

setFxParam :: FxParams -> FxName -> FxParamName -> Sig -> SE ()
setFxParam (FxParams nameMap) name param ins = do
  mapM_ (\ref -> writeRef ref ins) mParam
  where
    mParam = do
      paramMap <- Map.lookup name nameMap
      Map.lookup param paramMap

newFxParams :: [FxUnit] -> SE FxParams
newFxParams allUnits =
  FxParams . Map.fromList <$> mapM toNameMapItem allUnits
  where
    toNameMapItem :: FxUnit -> SE (FxName, ParamMap)
    toNameMapItem unit =
      (FxName (fxUnitName unit),) <$> toFxParamMap unit

toFxParamMap :: FxUnit -> SE ParamMap
toFxParamMap = newParamMap . toFxParamNameInitMap

toFxName :: FxUnit -> Text
toFxName = \case
  ToolFx config -> toolUnit.getName config
  ReverbFx config -> reverbUnit.getName config
  DelayFx config -> delayUnit.getName config
  PingPongFx config -> pingPongUnit.getName config
  MoogFx config -> moogUnit.getName config
  KorgFx config -> korgUnit.getName config
  BbcutFx config -> bbcutUnit.getName config
  LimiterFx config -> limiterUnit.getName config
  EqFx config -> eqUnit.getName config
  MixerEqFx config -> mixerEqUnit.getName config

toFxParamNameInitMap :: FxUnit -> ParamNameInitMap
toFxParamNameInitMap = \case
  ToolFx config -> toolUnit.getParams config
  ReverbFx config -> reverbUnit.getParams config
  DelayFx config -> delayUnit.getParams config
  PingPongFx config -> pingPongUnit.getParams config
  MoogFx config -> moogUnit.getParams config
  KorgFx config -> korgUnit.getParams config
  BbcutFx config -> bbcutUnit.getParams config
  LimiterFx config -> limiterUnit.getParams config
  EqFx config -> eqUnit.getParams config
  MixerEqFx config -> mixerEqUnit.getParams config

readParamMap :: FxName -> FxParams -> ParamMap
readParamMap name (FxParams nameMap) =
  fromMaybe (error errMessage) (Map.lookup name nameMap)
  where
    errMessage = "No FX unit is named with: " <> Text.unpack name.text

unitToFun :: Bpm -> ParamMap -> FxUnit -> Sig2 -> SE Sig2
unitToFun bpm params = \case
  ToolFx config -> toolUnit.apply bpm params config
  ReverbFx config -> reverbUnit.apply bpm params config
  DelayFx config -> delayUnit.apply bpm params config
  PingPongFx config -> pingPongUnit.apply bpm params config
  MoogFx config -> moogUnit.apply bpm params config
  KorgFx config -> korgUnit.apply bpm params config
  BbcutFx config -> bbcutUnit.apply bpm params config
  LimiterFx config -> limiterUnit.apply bpm params config
  EqFx config -> eqUnit.apply bpm params config
  MixerEqFx config -> mixerEqUnit.apply bpm params config

isBpmSensitive :: FxUnit -> Bool
isBpmSensitive = \case
  ToolFx _ -> toolUnit.needsBpm
  ReverbFx _ -> reverbUnit.needsBpm
  DelayFx _ -> delayUnit.needsBpm
  PingPongFx _ -> pingPongUnit.needsBpm
  MoogFx _ -> moogUnit.needsBpm
  KorgFx _ -> korgUnit.needsBpm
  BbcutFx _ -> bbcutUnit.needsBpm
  LimiterFx _ -> limiterUnit.needsBpm
  EqFx _ -> eqUnit.needsBpm
  MixerEqFx _ -> mixerEqUnit.needsBpm

fxUnitName :: FxUnit -> Text
fxUnitName = \case
  ToolFx config -> config.name
  ReverbFx config -> config.name
  DelayFx config -> config.name
  PingPongFx config -> config.name
  MoogFx config -> config.name
  KorgFx config -> config.name
  BbcutFx config -> config.name
  LimiterFx config -> config.name
  EqFx config -> config.name
  MixerEqFx config -> config.name
