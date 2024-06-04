module Live.Scene.Mixer.Route
  ( RouteCtx (..)
  , Channel
  , toRouteInstrBodies
  ) where

import Csound.Core
import Data.Maybe
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Live.Scene.Mixer.Route.DependencyGraph
import Live.Scene.Mixer.Config
import Live.Scene.Fx.Config
import Live.Scene.Fx (FxName (..), FxParams, Bpm, readParamMap, unitToFun)

type InstrBody = SE ()
type Channel = Int

data RouteDeps = RouteDeps
  { readChannel :: Channel -> SE Sig2
  , readChannelGain :: Channel -> SE Sig
  , readChannelSendGain :: Channel -> Channel -> SE Sig
  , writeChannel :: Channel -> Sig2 -> SE ()
  , appendChannel :: Channel -> Sig2 -> SE ()
  , appendMaster :: Sig2 -> SE ()
  }

newtype ChannelConfigMap = ChannelConfigMap (IntMap ChannelConfig)

getConfig :: ChannelConfigMap -> Channel -> Maybe ChannelConfig
getConfig (ChannelConfigMap configs) channel =
  IntMap.lookup channel configs

withConfig :: ChannelConfigMap -> Channel -> (ChannelConfig -> SE ()) -> SE ()
withConfig configs channel cont =
  maybe (pure ()) cont $ getConfig configs channel

data RouteCtx = RouteCtx
  { deps :: RouteDeps
  , configs :: ChannelConfigMap
  , bpm :: Bpm
  , fxParams :: FxParams
  }

toRouteInstrBodies :: RouteCtx -> Route -> [InstrBody]
toRouteInstrBodies ctx (Route groupActs) = do
  groupAct <- groupActs
  case extractFxAct groupAct of
    Right channel -> applyFx ctx channel
    Left acts -> pure $ mapM_ routeActToInstrBody acts
  where
    routeActToInstrBody :: RouteAct -> SE ()
    routeActToInstrBody act =
      case act.type_ of
        CopyOutput _output -> copyOutputInstrBody ctx act.channel
        CopySends -> copySends ctx act.channel
        ApplyFx -> sequence_ $ applyFx ctx act.channel

extractFxAct :: GroupAct -> Either [RouteAct] Int
extractFxAct (GroupAct acts) =
  case acts of
    [act] | act.type_ == ApplyFx -> Right act.channel
    _ -> Left acts

copyOutputInstrBody :: RouteCtx -> Channel -> SE ()
copyOutputInstrBody ctx channel = withConfig ctx.configs channel $ \config -> do
  gain <- ctx.deps.readChannelGain channel
  asig <- mul gain <$> ctx.deps.readChannel channel
  maybe
    (ctx.deps.appendMaster asig)
    (\output -> ctx.deps.appendChannel output asig)
    config.output

applyFx :: RouteCtx -> Channel -> [SE ()]
applyFx ctx channel =
  case getConfig ctx.configs channel of
    Nothing -> []
    Just config -> do
      fx <- concat $ maybeToList config.fxs
      let
        fxFun = unitToFun ctx.bpm (readParamMap (FxName fx.name) ctx.fxParams) fx.fx
      pure $ fxInstrBody ctx.deps channel fxFun

fxInstrBody :: RouteDeps -> Channel -> (Sig2 -> SE Sig2) -> SE ()
fxInstrBody RouteDeps{..} channel fun = do
  ins <- readChannel channel
  writeChannel channel =<< fun ins

copySends :: RouteCtx -> Channel -> SE ()
copySends ctx channel = withConfig ctx.configs channel $ \config -> do
  maybe
    (pure ())
    (\sends -> do
        asig <- ctx.deps.readChannel channel
        mapM_ (\send -> writeSend asig send.channel) sends)
    config.sends
  where
    writeSend :: Sig2 -> Channel -> SE ()
    writeSend channelOutput send = do
      gain <- ctx.deps.readChannelSendGain channel send
      ctx.deps.appendChannel send (mul gain channelOutput)
