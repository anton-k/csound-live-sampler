module Live.Scene.Fx
  ( Fx (..)
  , FxDeps (..)
  , FxParams (..)
  , modifyFxParam
  , newFxs
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
import Csound.Core.Opcodes.Fx (analogDelay)

newtype Bpm = Bpm (SE Sig)

data Fx = Fx FxConfig

-- | Connects FX-processor with the mixer
data FxDeps = FxDeps
  { readChannel :: Int -> SE Sig2
  , writeChannel :: Int -> Sig2 -> SE ()
  , readMaster :: SE Sig2
  , writeMaster :: Sig2 -> SE ()
  , readBpm :: SE Sig
  }

type FxName = Text
type FxParamName = Text
type ParamMap = Map FxParamName (Ref Sig)

newtype FxParams = FxParams (Map FxName ParamMap)

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
      (unit.name, ) <$> toParamMap unit.fx

    toParamMap :: FxUnit -> SE ParamMap
    toParamMap = \case
      ReverbFx config ->
        params config [("size", (.size)), ("dump", (.dump)), ("dryWet", (.dryWet))]
      DelayFx config ->
        params config [("repeatTime", (.repeatTime)), ("dump", (.dump)), ("feedback", (.feedback)), ("dryWet", (.dryWet))]
      PingPongFx config ->
        params config [("repeatTime1", (.repeatTime1)), ("repeatTime2", (.repeatTime2)),("dump", (.dump)), ("feedback", (.feedback)), ("dryWet", (.dryWet))]
      MoogFx config ->
        params config [("cutoff", (.cutoff)), ("resonance", (.resonance)), ("dryWet", (.dryWet))]
      KorgFx config ->
        params config [("cutoff", (.cutoff)), ("resonance", (.resonance)), ("dryWet", (.dryWet))]
      BbcutFx config ->
        params config [("dryWet", (.dryWet))]
      where
        params :: config -> [(FxParamName, (config -> Float))] -> SE ParamMap
        params config args =
          Map.fromList <$> mapM (param config) args

        param :: config -> (FxParamName, (config -> Float)) -> SE (FxParamName, Ref Sig)
        param config (name, extract) = do
          ref <- newCtrlRef $ float (extract config)
          pure (name, ref)

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
    outs <- unitToFun bpm (readParamMap unit.name params) unit.fx ins
    ref.write outs
  play instrId [Note 0 (-1) ()]
  pure instrId

readParamMap :: FxName -> FxParams -> ParamMap
readParamMap name (FxParams nameMap) =
  fromMaybe (error errMessage) (Map.lookup name nameMap)
  where
    errMessage = "No FX unit is named with: " <> Text.unpack name

unitToFun :: Bpm -> ParamMap -> FxUnit -> Sig2 -> SE Sig2
unitToFun bpm params = \case
  ReverbFx _config -> reverbFx params
  DelayFx _config -> delayFx bpm params
  PingPongFx config -> pingPongFx config
  MoogFx _config -> moogFx params
  KorgFx _config -> korgFx params
  BbcutFx config -> bbcutFx bpm params config

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
  ReverbFx _ -> False
  DelayFx _ -> True
  PingPongFx _ -> True
  MoogFx _ -> False
  KorgFx _ -> False
  BbcutFx _ -> True

-------------------------------------------------------------------------------------
-- FX units

readParam :: ParamMap -> FxParamName -> SE Sig
readParam params name =
  readRef ref
  where
    ref = fromMaybe (error errMessage) $ Map.lookup name params

    errMessage = "No required param: " <> Text.unpack name

reverbFx :: ParamMap -> Sig2 -> SE Sig2
reverbFx params ins = do
  dryWet <- param "dryWet"
  size <- param "size"
  dump <- param "dump"
  pure $ mixAt dryWet (reverbsc size dump) ins
  where
    param = readParam params

--   mixAt (float config.dryWet) (reverbsc (float config.size) (float config.dump)) ins

delayFx :: Bpm -> ParamMap -> Sig2 -> SE Sig2
delayFx bpm params ins = undefined

pingPongFx :: PingPongConfig -> Sig2 -> SE Sig2
pingPongFx = undefined

moogFx :: ParamMap -> Sig2 -> SE Sig2
moogFx params ins = do
  dryWet <- param "dryWet"
  cutoff <- param "cutoff"
  resonance <- param "resonance"
  pure $ mixAt dryWet (moogvcf2 cutoff resonance) ins
  where
    param = readParam params

-- mixAt (float config.dryWet) (moogvcf2 (float config.cutoff) (float config.resonance)) ins

korgFx :: ParamMap -> Sig2 -> SE Sig2
korgFx params ins = do
  dryWet <- param "dryWet"
  cutoff <- param "cutoff"
  resonance <- param "resonance"
  pure $ mixAt dryWet (k35_lpf cutoff resonance) ins
  where
    param = readParam params

--  mixAt (float config.dryWet) (k35_lpf (float config.cutoff) (float config.resonance)) ins

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
