module Live.Scene
  ( Scene
  , runScene
  , loadScene
  , TrackId
  , PartId
  , Clip
  ) where

import Live.Config
import Csound.Core
import System.FilePath ((</>))

runScene :: Config -> IO ()
runScene config =
  dacBy setMa {- writeCsd "tmp.csd" -} $ do
  -- writeCsd "tmp.csd" $ do
    scene <- loadScene config
    setupFaders config scene
    toAudio config scene

data Scene = Scene
  { master :: Master
  , channels :: [Channel]
  }

data Master = Master
  { volume :: Ref Sig
  }

data Channel = Channel
  { volume :: Ref Sig
  }

data TrackId
data PartId

data Clip = Clip
  { track :: TrackId
  , part :: PartId
  }

-- | Completes crrently played clip and switches
-- to the next Clip
playClip :: Scene -> Clip -> SE ()
playClip = undefined

data ChanId
data FxId

toAudio :: Config -> Scene -> SE Sig2
toAudio config scene = do
  tracks <- head <$> mapM (playTrack config.dir scene) config.tracks
  applyMaster scene.master tracks

playTrack :: Maybe FilePath -> Scene -> TrackConfig -> SE Sig2
playTrack sceneDir scene track =
  sum <$> mapM (playStem trackDir scene) track.stems
  where
    trackDir = addFilePrefix sceneDir <$> track.dir

playStem :: Maybe FilePath -> Scene -> Stem -> SE Sig2
playStem mDir scene stem = do
  vol <- readRef (scene.channels !! (stem.channel - 1)).volume
  pure $ mul (vol * maybe 1 float stem.volume) (loopWav file 1)
  where
    file = fromString $ addFilePrefix mDir stem.file

addFilePrefix :: Maybe FilePath -> FilePath -> FilePath
addFilePrefix mDir file = maybe id (</>) mDir file

applyMaster :: Master -> Sig2 -> SE Sig2
applyMaster master asig = do
  vol <- readRef master.volume
  pure $ mul vol asig

loadScene :: Config -> SE Scene
loadScene config =
  Scene
    <$> loadMaster
    <*> loadChannels
  where
    loadMaster =
      Master <$> newCtrlRef (float config.master.volume)

    loadChannels =
      mapM (fmap Channel . newCtrlRef . float . (.volume)) config.channels

setupFaders :: Config -> Scene -> SE ()
setupFaders config scene = do
  setupMaster fadersMidi config.master scene.master
  setupChannels fadersMidi config.channels scene.channels
  where
    fadersMidi = config.controllers.faders

setupChannels :: FadersMidiConfig -> [ChannelConfig] -> [Channel] -> SE ()
setupChannels fadersConfig configs channels =
  mapM_
    (\(chn, config, channel) -> setupChannel chn config channel)
    (zip3 midiChannels configs channels)
  where
    midiChannels = int <$> fadersConfig.channels

setupChannel :: D -> ChannelConfig -> Channel -> SE ()
setupChannel chn config channel =
  initFader chn config.volume channel.volume

setupMaster :: FadersMidiConfig -> MasterConfig -> Master -> SE ()
setupMaster fadersConfig config master =
  initFader (int fadersConfig.master) config.volume master.volume

initFader :: D -> Float -> Ref Sig -> SE ()
initFader chn initVal ref = do
  initc7  (CtrlInit 1 chn (float initVal))
  writeRef ref kVol
  where
    kVol = gainslider $ kr $ ctrl7 1 chn 0 127
