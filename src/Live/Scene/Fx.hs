module Live.Scene.Fx
  ( Fx (..)
  , FxDeps (..)
  , FxParams (..)
  , modifyFxParam
  , newMasterFxs
  , newChannelFxs
  , unitToFun
  ) where

import Control.Monad
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
      LimiterFx _config -> pure mempty
      EqFx config ->
        params config $ eqPointParams =<< [0..]
      where
        params :: config -> [(FxParamName, (config -> Float))] -> SE ParamMap
        params config args =
          Map.fromList <$> mapM (param config) args

        param :: config -> (FxParamName, (config -> Float)) -> SE (FxParamName, Ref Sig)
        param config (name, extract) = do
          ref <- newCtrlRef $ float (extract config)
          pure (name, ref)

        eqPointParams :: Int -> [(FxParamName, (EqConfig -> Float))]
        eqPointParams index =
          [ (nameIndex "frequency", \config -> (config.points !! index).frequency)
          , (nameIndex "gain", \config -> (config.points !! index).gain)
          , (nameIndex "width", \config -> fromMaybe 0 (config.points !! index).width)
          ]
          where
            nameIndex name = name <> Text.pack (show (index + 1))

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
  LimiterFx config -> limiterFx config
  EqFx config -> eqFx params config

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
  LimiterFx _ -> False
  EqFx _ -> False

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
  damp <- dampParam <$> param "damp"
  pure $ mixAt dryWet (reverbsc size damp) ins
  where
    param = readParam params

dampFrequencyRange :: Num a => (a, a)
dampFrequencyRange = (100, 14000)

dampParam :: Sig -> Sig
dampParam = frequencyParam dampFrequencyRange

frequencyParam :: (Sig, Sig) -> Sig -> Sig
frequencyParam (minVal, maxVal) param =
  scale (expcurve param 4) maxVal minVal

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

cutoffFrequencyRange :: Num a => (a, a)
cutoffFrequencyRange = (10, 20000)

cutoffParam :: Sig -> Sig
cutoffParam = frequencyParam cutoffFrequencyRange

moogFx :: ParamMap -> Sig2 -> SE Sig2
moogFx params ins = do
  dryWet <- param "dryWet"
  cutoff <- cutoffParam <$> param "cutoff"
  resonance <- param "resonance"
  pure $ mixAt dryWet (\ain -> moogvcf2 ain cutoff resonance) ins
  where
    param = readParam params

korgFx :: ParamMap -> Sig2 -> SE Sig2
korgFx params ins = do
  dryWet <- param "dryWet"
  cutoff <- cutoffParam <$> param "cutoff"
  resonance <- param "resonance"
  pure $ mixAt dryWet (\ain -> k35_lpf ain cutoff resonance) ins
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

limiterFx :: LimiterConfig -> Sig2 -> SE Sig2
limiterFx config ins =
  pure $ mul (float config.maxVolume) $
    at (\ain -> compress ain ain kthresh kloknee khiknee kratio katt krel ilook) ins
  where
    kthresh = 0
    kloknee = 95
    khiknee = 95
    kratio = 100
    katt = 0.005
    krel = 0.005
    ilook = 0

eqFx :: ParamMap -> EqConfig -> Sig2 -> SE Sig2
eqFx params config inputs =
  at eqUnit inputs
  where
    eqUnit :: Sig -> SE Sig
    eqUnit = foldr (>=>) pure $ zipWith applyPoint [1..] config.points

    applyPoint :: Int -> EqPoint -> Sig -> SE Sig
    applyPoint index point ain = do
      args <- readEqPointSig params index
      pure $ applyEq point.mode args ain

data EqPointSig = EqPointSig
  { frequency :: Sig
  , gain :: Sig
  , width :: Sig
  }

readEqPointSig :: ParamMap -> Int -> SE EqPointSig
readEqPointSig params index = do
  frequency <- param "frequency"
  gain <- param "gain"
  width <- param "width"
  pure EqPointSig { frequency, gain, width }
  where
    param name = readParam params (name <> Text.pack (show index))

applyEq :: EqMode -> EqPointSig -> Sig -> Sig
applyEq = undefined
