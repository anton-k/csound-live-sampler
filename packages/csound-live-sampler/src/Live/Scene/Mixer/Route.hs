module Live.Scene.Mixer.Route (
  MixerRoute (..),
  MixerRouteFx (..),
  RouteDeps (..),
  FxId (..),
  FxParamId (..),
  toMixerRoute,
  MixerInstrIds,
) where

import Csound.Core
import Data.Boolean ((==*))
import Data.Default
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Live.Scene.Common (ChannelId (..))
import Live.Scene.Mixer.Config
import Live.Scene.Mixer.Fx (Bpm (..), FxId (..), FxName (..), FxParamId, FxParams, readFxCtx, unitToFun)
import Live.Scene.Mixer.Fx qualified as Fx
import Live.Scene.Mixer.Fx.Config
import Live.Scene.Mixer.Route.DependencyGraph
import Prelude hiding (read)

toMixerRoute :: RouteDeps -> Bpm -> MixerConfig ChannelId -> SE MixerRoute
toMixerRoute deps bpm config = do
  ctx <- newRouteCtx deps config bpm
  let
    instrBodies = toRouteInstrBodies ctx route
  pure $
    MixerRoute
      { setupInstr =
          MixerInstrIds <$> mapM (toCsdInstr . getBody) instrBodies
      , launchInstr = \(MixerInstrIds instrIds) -> do
          mapM_ playCsdInstr instrIds
          reloadOnBpmChange bpm (bpmSensitiveInstrs instrBodies instrIds)
      , fxControls = initFxControls ctx instrBodies
      }
  where
    route :: Route
    route = orderDependencies config

    getBody :: MixerInstr -> SE ()
    getBody = \case
      ChannelMixerInstr instr -> instr.body
      FxMixerInstr instr -> instr.body

    -- instruments that we need to restart on bpm change
    bpmSensitiveInstrs :: [MixerInstr] -> [InstrRef ()] -> [InstrRef ()]
    bpmSensitiveInstrs bodies instrIds =
      fmap snd $ filter (isBpmSensitive . fst) $ zip bodies instrIds

isBpmSensitive :: MixerInstr -> Bool
isBpmSensitive = \case
  ChannelMixerInstr _ -> False
  FxMixerInstr instr -> instr.needsBpm

toCsdInstr :: SE () -> SE (InstrRef ())
toCsdInstr body = newProc $ const body

playCsdInstr :: InstrRef () -> SE ()
playCsdInstr instrRef = play instrRef [Note 0 (-1) ()]

stopCsdInstr :: InstrRef () -> SE ()
stopCsdInstr instrId = turnoff2 instrId 0 0.25

initFxControls :: RouteCtx -> [MixerInstr] -> MixerInstrIds -> MixerRouteFx
initFxControls ctx bodies (MixerInstrIds instrIds) =
  MixerRouteFx
    { modifyFxParam = modifyFxParamSt ctx.fxParams
    , setFxParam = setFxParamSt ctx.fxParams
    , readFxParam = readFxParamSt ctx.fxParams
    , toggleFxBypass = toggleFxBypassSt ctx.fxParams
    , startFx = \name -> withFxName name playCsdInstr
    , stopFx = \name -> withFxName name stopCsdInstr
    }
  where
    withFxName :: FxName -> (InstrRef () -> SE ()) -> SE ()
    withFxName name cont =
      mapM_ cont $ Map.lookup name nameToIdMap

    nameToIdMap :: Map FxName (InstrRef ())
    nameToIdMap = Map.fromList $ mapMaybe toItem $ zip bodies instrIds
      where
        toItem :: (MixerInstr, val) -> Maybe (FxName, val)
        toItem (instrBody, val) = case instrBody of
          FxMixerInstr instr -> Just (instr.name, val)
          ChannelMixerInstr _ -> Nothing

modifyFxParamSt :: FxParams -> FxParamId -> (Sig -> Sig) -> SE ()
modifyFxParamSt fxParams paramId f =
  Fx.modifyFxParam fxParams paramId f

setFxParamSt :: FxParams -> FxParamId -> Sig -> SE ()
setFxParamSt fxParams paramId ins =
  Fx.setFxParam fxParams paramId ins

readFxParamSt :: FxParams -> FxParamId -> SE Sig
readFxParamSt fxParams paramId =
  Fx.readFxParam fxParams paramId

toggleFxBypassSt :: FxParams -> FxId -> SE ()
toggleFxBypassSt fxParams fxId =
  Fx.toggleFxBypass fxParams fxId

newRouteCtx :: RouteDeps -> MixerConfig ChannelId -> Bpm -> SE RouteCtx
newRouteCtx deps config bpm = do
  fxParams <- initFxParams config
  pure $ RouteCtx{deps, configs = configMap, masterConfig = fromMaybe def config.master, bpm, fxParams}
  where
    configMap = initConfigMap config

initConfigMap :: MixerConfig ChannelId -> ChannelConfigMap
initConfigMap config =
  ChannelConfigMap $ IntMap.fromList $ zip [0 ..] config.channels

initFxParams :: MixerConfig ChannelId -> SE FxParams
initFxParams config =
  Fx.newFxParams allFxs
  where
    allFxs = do
      mUnits <- masterFx : zipWith channelFx [0 ..] config.channels
      maybeToList mUnits

    masterFx = fmap (Nothing,) $ (fromMaybe def config.master).fxs

    channelFx channelId channel = fmap (Just (ChannelId channelId),) channel.fxs

reloadOnBpmChange :: Bpm -> [InstrRef ()] -> SE ()
reloadOnBpmChange (Bpm readBpm) instrRefs
  | null instrRefs = pure ()
  | otherwise = do
      bpm <- readBpm
      when1 (changed [bpm] ==* 1) $
        mapM_ restartFxInstr instrRefs

restartFxInstr :: InstrRef () -> SE ()
restartFxInstr instrId = do
  turnoff2 instrId 0 0.25
  play instrId [Note 0 (-1) ()]

data RouteDeps = RouteDeps
  { readChannel :: ChannelId -> SE Sig2
  , readChannelVolume :: ChannelId -> SE Sig
  , readChannelSendGain :: ChannelId -> ChannelId -> SE Sig
  , writeChannel :: ChannelId -> Sig2 -> SE ()
  , appendChannel :: ChannelId -> Sig2 -> SE ()
  , readMaster :: SE Sig2
  , writeMaster :: Sig2 -> SE ()
  , appendMaster :: Sig2 -> SE ()
  }

newtype ChannelConfigMap = ChannelConfigMap (IntMap (ChannelConfig ChannelId))

getConfig :: ChannelConfigMap -> ChannelId -> Maybe (ChannelConfig ChannelId)
getConfig (ChannelConfigMap configs) (ChannelId channel) =
  IntMap.lookup channel configs

withConfig :: ChannelConfigMap -> ChannelId -> (ChannelConfig ChannelId -> SE ()) -> SE ()
withConfig configs channel cont =
  maybe (pure ()) cont $ getConfig configs channel

data RouteCtx = RouteCtx
  { deps :: RouteDeps
  , configs :: ChannelConfigMap
  , masterConfig :: MasterConfig
  , bpm :: Bpm
  , fxParams :: FxParams
  }

data MixerInstr
  = ChannelMixerInstr ChannelInstr
  | FxMixerInstr FxInstr

data ChannelInstr = ChannelInstr
  { body :: SE ()
  }

data FxInstr = FxInstr
  { body :: SE ()
  , needsBpm :: Bool
  , name :: FxName
  }

newtype MixerInstrIds = MixerInstrIds [InstrRef ()]

data MixerRoute = MixerRoute
  { setupInstr :: SE MixerInstrIds
  , launchInstr :: MixerInstrIds -> SE ()
  , fxControls :: MixerInstrIds -> MixerRouteFx
  }

data MixerRouteFx = MixerRouteFx
  { modifyFxParam :: FxParamId -> (Sig -> Sig) -> SE ()
  , setFxParam :: FxParamId -> Sig -> SE ()
  , readFxParam :: FxParamId -> SE Sig
  , toggleFxBypass :: FxId -> SE ()
  , startFx :: FxName -> SE ()
  , stopFx :: FxName -> SE ()
  }

toRouteInstrBodies :: RouteCtx -> Route -> [MixerInstr]
toRouteInstrBodies ctx route =
  toRouteChannelInstrBodies ctx route <> toRouteMasterInstrBodies ctx

toRouteMasterInstrBodies :: RouteCtx -> [MixerInstr]
toRouteMasterInstrBodies ctx = do
  fx <- concat $ maybeToList ctx.masterConfig.fxs
  pure $ FxMixerInstr (unitToFxInstr ctx Nothing ctx.deps.readMaster ctx.deps.writeMaster fx)

toRouteChannelInstrBodies :: RouteCtx -> Route -> [MixerInstr]
toRouteChannelInstrBodies ctx (Route groupActs) = do
  groupAct <- groupActs
  case extractFxAct groupAct of
    Right channel -> fmap FxMixerInstr $ applyFx ctx channel
    Left acts -> pure $ ChannelMixerInstr $ ChannelInstr $ mapM_ routeActToInstrBody acts
  where
    routeActToInstrBody :: RouteAct -> SE ()
    routeActToInstrBody act =
      case act.type_ of
        CopyOutput _output -> copyOutputInstrBody ctx act.channel
        CopySends -> copySends ctx act.channel
        ApplyFx -> mapM_ (.body) $ applyFx ctx act.channel

extractFxAct :: GroupAct -> Either [RouteAct] ChannelId
extractFxAct (GroupAct acts) =
  case acts of
    [act] | act.type_ == ApplyFx -> Right act.channel
    _ -> Left acts

copyOutputInstrBody :: RouteCtx -> ChannelId -> SE ()
copyOutputInstrBody ctx channel = withConfig ctx.configs channel $ \config -> do
  gain <- ctx.deps.readChannelVolume channel
  asig <- mul gain <$> ctx.deps.readChannel channel
  maybe
    (ctx.deps.appendMaster asig)
    (\output -> ctx.deps.appendChannel output asig)
    config.output

applyFx :: RouteCtx -> ChannelId -> [FxInstr]
applyFx ctx channel =
  case getConfig ctx.configs channel of
    Nothing -> []
    Just config -> do
      fx <- concat $ maybeToList config.fxs
      pure (unitToFxInstr ctx (Just channel) (ctx.deps.readChannel channel) (ctx.deps.writeChannel channel) fx)

unitToFxInstr :: RouteCtx -> Maybe ChannelId -> SE Sig2 -> (Sig2 -> SE ()) -> FxUnit -> FxInstr
unitToFxInstr ctx mChannel read write fx =
  FxInstr
    { body = fxInstrBody read write fxFun
    , needsBpm = Fx.isBpmSensitive fx
    , name = fxName
    }
  where
    fxName = FxName fx.name
    fxFun = applyBypass fxCtx.bypass (unitToFun ctx.bpm fxCtx.params fx)
    fxCtx = readFxCtx mChannel fxName ctx.fxParams

applyBypass :: Ref Sig -> (Sig2 -> SE Sig2) -> Sig2 -> SE Sig2
applyBypass bypassRef f input = do
  output <- f input
  bypass <- readRef bypassRef
  pure (ifB (bypass ==* 1) input output)

fxInstrBody :: SE Sig2 -> (Sig2 -> SE ()) -> (Sig2 -> SE Sig2) -> SE ()
fxInstrBody read write fun = do
  ins <- read
  write =<< fun ins

copySends :: RouteCtx -> ChannelId -> SE ()
copySends ctx channel = withConfig ctx.configs channel $ \config -> do
  maybe
    (pure ())
    ( \sends -> do
        asig <- ctx.deps.readChannel channel
        mapM_ (\send -> writeSend asig send.channel) sends
    )
    config.sends
  where
    writeSend :: Sig2 -> ChannelId -> SE ()
    writeSend channelOutput send = do
      gain <- ctx.deps.readChannelSendGain channel send
      ctx.deps.appendChannel send (mul gain channelOutput)
