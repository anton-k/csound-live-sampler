module Live.Scene.Fx
  ( Fx (..)
  , FxDeps (..)
  , FxName (..)
  , FxParams (..)
  , modifyFxParam
  , newMasterFxs
  , newChannelFxs
  , unitToFun
  ) where

import Live.Scene.Fx.Config
import Data.Boolean ((==*))
import Csound.Core
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Maybe
import Live.Scene.Fx.Unit
import Live.Scene.Fx.Unit.Reverb (reverbUnit)
import Live.Scene.Fx.Unit.Delay (delayUnit, pingPongUnit)
import Live.Scene.Fx.Unit.Filter (moogUnit, korgUnit)
import Live.Scene.Fx.Unit.Eq (eqUnit, mixerEqUnit)
import Live.Scene.Fx.Unit.Compress (limiterUnit)
import Live.Scene.Fx.Unit.Bbcuts (bbcutUnit)

data Fx = Fx FxConfig

-- | Connects FX-processor with the mixer
data FxDeps = FxDeps
  { readChannel :: Int -> SE Sig2
  , writeChannel :: Int -> Sig2 -> SE ()
  , readMaster :: SE Sig2
  , writeMaster :: Sig2 -> SE ()
  , readBpm :: SE Sig
  }

newtype FxName = FxName { text :: Text }
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

newFxParams :: [FxConfig] -> SE FxParams
newFxParams configs =
  FxParams . Map.fromList <$> mapM toNameMapItem allUnits
  where
    allUnits :: [NamedFx FxUnit]
    allUnits = (.chain) =<< configs

    toNameMapItem :: NamedFx FxUnit -> SE (FxName, ParamMap)
    toNameMapItem unit =
      (FxName unit.name, ) <$> toParamMap unit.fx

    toParamMap :: FxUnit -> SE ParamMap
    toParamMap = \case
      ReverbFx config -> reverbUnit.getParams config
      DelayFx config -> delayUnit.getParams config
      PingPongFx config -> pingPongUnit.getParams config
      MoogFx config -> moogUnit.getParams config
      KorgFx config -> korgUnit.getParams config
      BbcutFx config -> bbcutUnit.getParams config
      LimiterFx config -> limiterUnit.getParams config
      EqFx config -> eqUnit.getParams config
      MixerEqFx config -> mixerEqUnit.getParams config

newMasterFxs :: FxDeps -> [FxConfig] -> SE FxParams
newMasterFxs env configs =
  newFxs env (filter isMasterFx configs)

newChannelFxs :: FxDeps -> [FxConfig] -> SE FxParams
newChannelFxs env configs =
  newFxs env (filter (not . isMasterFx) configs)

isMasterFx :: FxConfig -> Bool
isMasterFx config =
  case config.input of
    MasterFx -> True
    _ -> False

newFxs :: FxDeps -> [FxConfig] -> SE FxParams
newFxs env configs = do
  params <- newFxParams configs
  instrBpmIds <- fmap concat $ mapM (newFx params env) configs
  reloadOnBpmChange env instrBpmIds
  pure params

reloadOnBpmChange :: FxDeps -> [InstrRef ()] -> SE ()
reloadOnBpmChange env instrRefs = do
  bpm <- env.readBpm
  when1 (changed [bpm] ==* 1) $
    mapM_ restartFxInstr instrRefs

restartFxInstr :: InstrRef () -> SE ()
restartFxInstr instrId = do
  turnoff2 instrId 0 0.25
  play instrId [Note 0 (-1) ()]

-- | It returns instrument ids which should be reloaded on change of the global BPM
newFx :: FxParams -> FxDeps -> FxConfig -> SE [InstrRef ()]
newFx params env config = do
  let
    ref = newFxRef env config.input
  instrIds <- mapM (\x -> (x.fx, ) <$> launchFx params (Bpm env.readBpm) ref x) config.chain
  pure $ fmap snd $ filter (isBpmSensitive . fst) instrIds

launchFx :: FxParams -> Bpm -> FxRef -> NamedFx FxUnit -> SE (InstrRef ())
launchFx params bpm ref unit = do
  instrId <- newProc $ \() -> do
    ins <- ref.read
    outs <- unitToFun bpm (readParamMap (FxName unit.name) params) unit.fx ins
    ref.write outs
  play instrId [Note 0 (-1) ()]
  pure instrId

readParamMap :: FxName -> FxParams -> ParamMap
readParamMap name (FxParams nameMap) =
  fromMaybe (error errMessage) (Map.lookup name nameMap)
  where
    errMessage = "No FX unit is named with: " <> Text.unpack name.text

unitToFun :: Bpm -> ParamMap -> FxUnit -> Sig2 -> SE Sig2
unitToFun bpm params = \case
  ReverbFx config -> reverbUnit.apply bpm params config
  DelayFx config -> delayUnit.apply bpm params config
  PingPongFx config -> pingPongUnit.apply bpm params config
  MoogFx config -> moogUnit.apply bpm params config
  KorgFx config -> korgUnit.apply bpm params config
  BbcutFx config -> bbcutUnit.apply bpm params config
  LimiterFx config -> limiterUnit.apply bpm params config
  EqFx config -> eqUnit.apply bpm params config
  MixerEqFx config -> mixerEqUnit.apply bpm params config

data FxRef = FxRef
  { write :: Sig2 -> SE ()
  , read :: SE Sig2
  }

newFxRef :: FxDeps -> FxInputType -> FxRef
newFxRef env = \case
  MasterFx ->
    FxRef
      { write = env.writeMaster
      , read = env.readMaster
      }

  ChannelFx (ChannelFxConfig channelId) ->
    FxRef
      { write = env.writeChannel channelId
      , read = env.readChannel channelId
      }

  GroupFx (GroupFxConfig ins out) ->
    FxRef
      { write = env.writeChannel out
      , read = fmap sum $ mapM (readFxGroupInput env) ins
      }

readFxGroupInput :: FxDeps -> FxChannelInput -> SE Sig2
readFxGroupInput env input =
  fmap (mul $ float @Sig input.gain) $ env.readChannel input.channel

isBpmSensitive :: FxUnit -> Bool
isBpmSensitive = \case
  ReverbFx _ -> reverbUnit.needsBpm
  DelayFx _ -> delayUnit.needsBpm
  PingPongFx _ -> pingPongUnit.needsBpm
  MoogFx _ -> moogUnit.needsBpm
  KorgFx _ -> korgUnit.needsBpm
  BbcutFx _ -> bbcutUnit.needsBpm
  LimiterFx _ -> limiterUnit.needsBpm
  EqFx _ -> eqUnit.needsBpm
  MixerEqFx _ -> mixerEqUnit.needsBpm
