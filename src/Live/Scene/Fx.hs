module Live.Scene.Fx
  ( Fx (..)
  , FxDeps (..)
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
import Csound.Core.Opcodes.Fx (analogDelay, pingPongDelay)

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
      (unit.name, ) <$> toParamMap unit.fx

    toParamMap :: FxUnit -> SE ParamMap
    toParamMap = \case
      ReverbFx config ->
        params config [("size", (.size)), ("damp", (.damp)), ("dryWet", (.dryWet))]
      DelayFx config ->
        params config [("damp", (.damp)), ("feedback", (.feedback)), ("dryWet", (.dryWet))]
      PingPongFx config ->
        params config [("damp", (.damp)), ("feedback", (.feedback)), ("width", (.width)), ("dryWet", (.dryWet))]
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

newMasterFxs :: FxDeps -> [FxConfig] -> SE FxParams
newMasterFxs env configs = newFxs env (filter isMasterFx configs)

newChannelFxs :: FxDeps -> [FxConfig] -> SE FxParams
newChannelFxs env configs = newFxs env (filter (not . isMasterFx) configs)

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
  DelayFx config -> delayFx bpm params config
  PingPongFx config -> pingPongFx bpm params config
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
  damp <- rescaleDamp <$> param "damp"
  pure $ mixAt dryWet (reverbsc size damp) ins
  where
    param = readParam params

rescaleDamp :: Sig -> Sig
rescaleDamp param = scale (expcurve param 4) 14000 100

delayFx :: Bpm -> ParamMap -> DelayConfig -> Sig2 -> SE Sig2
delayFx (Bpm readBpm) params config ins = do
  bpm <- toD . ir <$> readBpm
  dryWet <- param "dryWet"
  feedback <- param "feedback"
  damp <- param "damp"
  let
    time = toSig (toDelayTime bpm (float config.repeatTime))
  pure $ at (\x -> analogDelay x dryWet time feedback damp) ins
  where
    param = readParam params

toDelayTime :: D -> D -> D
toDelayTime bpm beats = 4 * beats * 60 / bpm

pingPongFx :: Bpm -> ParamMap -> PingPongConfig -> Sig2 -> SE Sig2
pingPongFx (Bpm readBpm) params config ins = do
  bpm <- toD . ir <$> readBpm
  dryWet <- param "dryWet"
  feedback <- param "feedback"
  width <- param "width"
  damp <- param "damp"
  let
    time = toDelayTime bpm (float config.repeatTime)
  pure $ pingPongDelay ins (toSig time) feedback dryWet width damp (time * 2)
  where
    param = readParam params

moogFx :: ParamMap -> Sig2 -> SE Sig2
moogFx params ins = do
  dryWet <- param "dryWet"
  cutoff <- param "cutoff"
  resonance <- param "resonance"
  pure $ mixAt dryWet (moogvcf2 cutoff resonance) ins
  where
    param = readParam params

korgFx :: ParamMap -> Sig2 -> SE Sig2
korgFx params ins = do
  dryWet <- param "dryWet"
  cutoff <- param "cutoff"
  resonance <- param "resonance"
  pure $ mixAt dryWet (k35_lpf cutoff resonance) ins
  where
    param = readParam params

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
