module Live.Scene.AudioCard
  ( AudioCard (..)
  , AudioCardDeps (..)
  , newAudioCard
  ) where

import Data.Maybe
import Csound.Core
import Live.Scene.Common (ChannelId (..), AudioInputId (..))
import Live.Scene.AudioCard.Config
import Safe

-- | Audio IO
data AudioCard = AudioCard
  { setInputGain :: AudioInputId -> Sig -> SE ()
  , setupOutputs :: SE ()
  }

data AudioCardDeps = AudioCardDeps
  { writeChannel :: ChannelId -> Sig2  -> SE ()
  , readChannel :: ChannelId -> SE Sig2
  , readMaster :: SE Sig2
  }

newAudioCard  :: AudioConfig ChannelId -> AudioCardDeps -> SE AudioCard
newAudioCard config deps = do
  st <- newSt config
  updateAudio st deps
  pure $ AudioCard
    { setInputGain = setInputGainSt st
    , setupOutputs = setupOutputsSt st deps
    }

-- | private state for Audio unit
data St = St
  { inputs :: [AudioInput]
  , outputs :: [AudioOutput]
  }

data AudioInput = AudioInput
  { channel :: ChannelId
  , cardInputId :: CardPortId
  , gain :: Ref Sig
  }

data OutputTo = ToMaster | ToChannel ChannelId

data AudioOutput = AudioOutput
  { channel :: OutputTo
  , cardOuptutId :: Maybe CardPortId
  , gain :: Ref Sig
  }

data CardPortId
  = MonoPort Int
  | StereoPort Int Int

newSt :: AudioConfig ChannelId -> SE St
newSt config = do
  inputs <- mapM newAudioInput (fromMaybe [] config.inputs)
  outputs <- mapM newAudioOutput (fromMaybe [defaultMasterOutput] config.outputs)
  pure St { inputs, outputs }

defaultMasterOutput :: AudioOutputConfig ChannelId
defaultMasterOutput =
  StereoAudioOutputConfig $ StereoOutputConfig
    { name = Just "master"
    , channel = Nothing -- output to master
    , gain = Nothing
    , stereo = Nothing -- use default card output
    }

newAudioInput :: AudioInputConfig ChannelId -> SE AudioInput
newAudioInput config =
  AudioInput (getChannelIdConfig config) (getCardInputId config) <$>
    newCtrlRef (maybe 1 float (getInputGainConfig config))

newAudioOutput :: AudioOutputConfig ChannelId -> SE AudioOutput
newAudioOutput config =
  AudioOutput (getOutputTo config) (getCardOutputId config) <$>
    newCtrlRef (maybe 1 float (getOutputGainConfig config))

getInputGainConfig :: AudioInputConfig a -> Maybe Float
getInputGainConfig = \case
  StereoAudioInputConfig config -> config.gain
  MonoAudioInputConfig config -> config.gain

getChannelIdConfig :: AudioInputConfig ChannelId -> ChannelId
getChannelIdConfig = \case
  StereoAudioInputConfig config -> config.channel
  MonoAudioInputConfig config -> config.channel

getCardInputId :: AudioInputConfig a -> CardPortId
getCardInputId = \case
  StereoAudioInputConfig config -> StereoPort (fst config.stereo) (snd config.stereo)
  MonoAudioInputConfig config -> MonoPort config.mono

getOutputTo :: AudioOutputConfig ChannelId -> OutputTo
getOutputTo = \case
  StereoAudioOutputConfig config -> toOutputTo config.channel
  MonoAudioOutputConfig config -> toOutputTo config.channel
  where
    toOutputTo = maybe ToMaster ToChannel

getCardOutputId :: AudioOutputConfig ChannelId -> Maybe CardPortId
getCardOutputId = \case
  StereoAudioOutputConfig config -> fmap (uncurry StereoPort) config.stereo
  MonoAudioOutputConfig config -> Just $ MonoPort $ config.mono

getOutputGainConfig :: AudioOutputConfig a -> Maybe Float
getOutputGainConfig = \case
  StereoAudioOutputConfig config -> config.gain
  MonoAudioOutputConfig config -> config.gain

setInputGainSt :: St -> AudioInputId -> Sig -> SE ()
setInputGainSt st (AudioInputId inputId) value =
  mapM_ (\input -> writeRef input.gain value) (st.inputs `atMay` inputId )

updateAudio :: St -> AudioCardDeps -> SE ()
updateAudio st deps = do
  instrRef <- newProc (\() -> updateAudioInstr st deps)
  play instrRef [Note 0 (-1) ()]

updateAudioInstr :: St -> AudioCardDeps -> SE ()
updateAudioInstr st deps =
  mapM_ (runInput deps) st.inputs

runInput :: AudioCardDeps -> AudioInput -> SE ()
runInput deps input = do
  gain <- readRef input.gain
  deps.writeChannel input.channel . mul gain =<< readInput input

readInput :: AudioInput -> SE Sig2
readInput input =
  case input.cardInputId of
    MonoPort n -> readMono n
    StereoPort left right -> readStereo left right

readMono :: Int -> SE Sig2
readMono n =
  fromMono <$> inch (int n)

readStereo :: Int -> Int -> SE Sig2
readStereo left right =
  (,) <$> inch (int left) <*> inch (int right)

setupOutputsSt :: St -> AudioCardDeps -> SE ()
setupOutputsSt st deps =
  mapM_ (runOutput deps) st.outputs

runOutput :: AudioCardDeps -> AudioOutput -> SE ()
runOutput deps output = do
  gain <- readRef output.gain
  writeOut . mul gain =<< readChannel
  where
    readChannel :: SE Sig2
    readChannel =
      case output.channel of
        ToMaster -> deps.readMaster
        ToChannel channelId -> deps.readChannel channelId

    writeOut :: Sig2 -> SE ()
    writeOut =
      case output.cardOuptutId of
        Nothing -> outs
        Just cardPort ->
          case cardPort of
            MonoPort n -> outch (int n) . toMono
            StereoPort a b -> \(sigA, sigB) -> do
              outch (int a) sigA
              outch (int b) sigB
