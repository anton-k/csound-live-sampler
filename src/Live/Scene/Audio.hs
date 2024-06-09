module Live.Scene.Audio
  ( Audio (..)
  , AudioDeps (..)
  , newAudio
  ) where

import Data.Maybe
import Csound.Core
import Live.Scene.Common (ChannelId (..), AudioInputId (..))
import Live.Scene.Audio.Config
import Safe

-- | Audio IO
data Audio = Audio
  { setInputGain :: AudioInputId -> Sig -> SE ()
  }

newtype AudioDeps = AudioDeps
  { writeChannel :: ChannelId -> Sig2  -> SE ()
  }

newAudio  :: AudioConfig ChannelId -> AudioDeps -> SE Audio
newAudio config deps = do
  st <- newSt config
  updateAudio st deps
  pure $ Audio
    { setInputGain = setInputGainSt st
    }

-- | private state for Audio unit
data St = St
  { inputs :: [AudioInput]
  }

data AudioInput = AudioInput
  { channel :: ChannelId
  , cardInputId :: CardInputId
  , gain :: Ref Sig
  }

data CardInputId
  = MonoInput Int
  | StereoInput Int Int

newSt :: AudioConfig ChannelId -> SE St
newSt config = do
  inputs <- mapM newAudioInput (fromMaybe [] config.inputs)
  pure St { inputs }

newAudioInput :: AudioInputConfig ChannelId -> SE AudioInput
newAudioInput config =
  AudioInput (getChannelIdConfig config) (getCardInputId config) <$>
    newCtrlRef (maybe 1 float (getGainConfig config))

getGainConfig :: AudioInputConfig a -> Maybe Float
getGainConfig = \case
  StereoAudioInputConfig config -> config.gain
  MonoAudioInputConfig config -> config.gain

getChannelIdConfig :: AudioInputConfig ChannelId -> ChannelId
getChannelIdConfig = \case
  StereoAudioInputConfig config -> config.channel
  MonoAudioInputConfig config -> config.channel

getCardInputId :: AudioInputConfig a -> CardInputId
getCardInputId = \case
  StereoAudioInputConfig config -> StereoInput (fst config.stereo) (snd config.stereo)
  MonoAudioInputConfig config -> MonoInput config.mono

setInputGainSt :: St -> AudioInputId -> Sig -> SE ()
setInputGainSt st (AudioInputId inputId) value =
  mapM_ (\input -> writeRef input.gain value) (st.inputs `atMay` inputId )

updateAudio :: St -> AudioDeps -> SE ()
updateAudio st deps = do
  instrRef <- newProc (\() -> updateAudioInstr st deps)
  play instrRef [Note 0 (-1) ()]

updateAudioInstr :: St -> AudioDeps -> SE ()
updateAudioInstr st deps =
  mapM_ (runInput deps) st.inputs

runInput :: AudioDeps -> AudioInput -> SE ()
runInput deps input = do
  gain <- readRef input.gain
  deps.writeChannel input.channel . mul gain =<< readInput input

readInput :: AudioInput -> SE Sig2
readInput input =
  case input.cardInputId of
    MonoInput n -> readMono n
    StereoInput left right -> readStereo left right

readMono :: Int -> SE Sig2
readMono n =
  fromMono <$> inch (int n)

readStereo :: Int -> Int -> SE Sig2
readStereo left right =
  (,) <$> inch (int left) <*> inch (int right)
