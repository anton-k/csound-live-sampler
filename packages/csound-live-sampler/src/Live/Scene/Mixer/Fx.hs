module Live.Scene.Mixer.Fx (
  FxId (..),
  FxParamId (..),
  FxName (..),
  FxParams (..),
  FxCtx (..),
  readFxCtx,
  modifyFxParam,
  setFxParam,
  readFxParam,
  toggleFxBypass,
  readFxBypass,
  getFxParamRef,
  unitToFun,
  Bpm (..),
  toFxParamMap,
  toFxParamNameInitMap,
  isBpmSensitive,
  newFxParams,
) where

import Csound.Core
import Data.Boolean ((==*))
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Live.Scene.Common (ChannelId (..))
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

newtype FxParams = FxParams (Map (Maybe ChannelId) (Map FxName FxCtx))
  deriving newtype (Semigroup, Monoid)

data FxCtx = FxCtx
  { params :: ParamMap
  , bypass :: Ref Sig
  }

data FxId = FxId
  { channel :: Maybe ChannelId
  , name :: Text
  }

data FxParamId = FxParamId
  { channel :: Maybe ChannelId
  , name :: Text
  , param :: Text
  }

getFxParamRef :: FxParams -> FxParamId -> Maybe (Ref Sig)
getFxParamRef (FxParams params) paramId = do
  nameMap <- Map.lookup paramId.channel params
  fxCtx <- Map.lookup (FxName paramId.name) nameMap
  Map.lookup paramId.param fxCtx.params

getFxBypassRef :: FxParams -> FxId -> Maybe (Ref Sig)
getFxBypassRef (FxParams params) fxId = do
  nameMap <- Map.lookup fxId.channel params
  fxCtx <- Map.lookup (FxName fxId.name) nameMap
  pure fxCtx.bypass

modifyFxParam :: FxParams -> FxParamId -> (Sig -> Sig) -> SE ()
modifyFxParam params paramId f = do
  mapM_ (\ref -> modifyRef ref f) (getFxParamRef params paramId)

setFxParam :: FxParams -> FxParamId -> Sig -> SE ()
setFxParam params paramId ins = do
  mapM_ (\ref -> writeRef ref ins) (getFxParamRef params paramId)

readFxParam :: FxParams -> FxParamId -> SE Sig
readFxParam params paramId =
  maybe (pure 0) readRef (getFxParamRef params paramId)

toggleFxBypass :: FxParams -> FxId -> SE ()
toggleFxBypass params fxId =
  forM_ (getFxBypassRef params fxId) $ \ref -> do
    value <- readRef ref
    whens [(value ==* 1, writeRef ref 0)] (writeRef ref 1)

readFxBypass :: FxParams -> FxId -> SE Sig
readFxBypass params fxId =
  maybe (pure 0) readRef (getFxBypassRef params fxId)

newFxParams :: [(Maybe ChannelId, [FxUnit])] -> SE FxParams
newFxParams allUnits =
  FxParams . fmap Map.fromList <$> mapM (mapM toNameMapItem) (Map.fromList allUnits)
  where
    toNameMapItem :: FxUnit -> SE (FxName, FxCtx)
    toNameMapItem unit = do
      paramMap <- toFxParamMap unit
      bypassRef <- newCtrlRef (maybe 0 (\isBypass -> if isBypass then 1 else 0) unit.bypass)
      pure
        ( FxName unit.name
        , FxCtx
            { params = paramMap
            , bypass = bypassRef
            }
        )

toFxParamMap :: FxUnit -> SE ParamMap
toFxParamMap = newParamMap . toFxParamNameInitMap

toFxParamNameInitMap :: FxUnit -> ParamNameInitMap
toFxParamNameInitMap unit = case unit.mode of
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

readFxCtx :: Maybe ChannelId -> FxName -> FxParams -> FxCtx
readFxCtx mChannel name (FxParams params) =
  fromMaybe (error errMessage) (Map.lookup name =<< Map.lookup mChannel params)
  where
    errMessage = "No FX unit is named with: " <> Text.unpack name.text

unitToFun :: Bpm -> ParamMap -> FxUnit -> Sig2 -> SE Sig2
unitToFun bpm params unit = case unit.mode of
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
isBpmSensitive unit =
  case unit.mode of
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
