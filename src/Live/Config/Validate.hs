module Live.Config.Validate
  ( validateConfig
  ) where

import Control.Monad
import Control.Monad.Writer.Strict
import Data.Maybe
import System.Directory
import System.FilePath
import Live.Config.Types
import Data.Text (Text)
import Data.Text qualified as Text
import Live.Scene.Sampler.Config
import GHC.Records
import Live.Scene.Mixer.Config
import Live.Scene.Fx.Config

-- | Nothing if everythong is ok
--
-- Checks that
--
-- * all files for stems exist
-- * volumes are within useful range
-- * audio and controllers are valid
-- * valid channels are used for stems
validateConfig :: Config -> IO (Maybe Text)
validateConfig config = runValid $ do
  checkFiles config
  checkVolumes config
  checkAudio config
  checkControllers config
  checkChannels config
  where
    checkAudio _ = pure () -- TODO
    checkControllers _ = pure () -- TODO

-------------------------------------------------------------------------------------
-- files

checkFiles :: Config -> Valid ()
checkFiles config =
  checkSamplerFiles config.sampler

-- | Validation monad (it accumulates error messages)
type Valid a = WriterT [Text] IO a

runValid :: Valid () -> IO (Maybe Text)
runValid valid = do
  errs <- execWriterT valid
  pure $ if null errs
    then Nothing
    else Just (Text.unlines $ fmap (<> "\n") errs)

checkSamplerFiles :: SamplerConfig -> Valid ()
checkSamplerFiles config =
  withRoot config Nothing $ \root -> do
    mapM_ (checkTrack root) config.tracks
    mapM_ (checkClips root) config.clips

checkDir :: Maybe FilePath -> Valid Bool
checkDir = \case
  Nothing -> pure True
  Just dir -> do
    absDir <- liftIO $ makeAbsolute dir
    isOk <- liftIO $ doesDirectoryExist absDir
    unless isOk $ directoryDoesNotExist absDir
    pure isOk

checkFile :: FilePath -> Valid ()
checkFile file = do
    absFile <- liftIO $ makeAbsolute file
    isOk <- liftIO $ doesFileExist absFile
    unless isOk $ fileDoesNotExist absFile

checkTrack :: Maybe FilePath -> TrackConfig -> Valid ()
checkTrack mRoot config = do
  withRoot config mRoot $ \root ->
    mapM_ (checkStemFile root) config.stems

checkStemFile :: Maybe FilePath -> StemConfig -> Valid ()
checkStemFile mRoot config =
  checkFile (appendPath mRoot config.file)

appendMaybePath :: Maybe FilePath -> Maybe FilePath -> Maybe FilePath
appendMaybePath mA mB = case mA of
  Just a -> fmap (a </> ) mB
  Nothing -> mB

appendPath :: Maybe FilePath -> FilePath -> FilePath
appendPath mPrefix = maybe id (</>)mPrefix

checkClips :: Maybe FilePath -> ClipsConfig -> Valid ()
checkClips mRoot config =
  withRoot config mRoot $ \root ->
    mapM_ (checkClipColumnFiles root) config.columns

checkClipColumnFiles :: Maybe FilePath -> ClipColumnConfig -> Valid ()
checkClipColumnFiles mRoot config = do
  withRoot config mRoot $ \root ->
    mapM_ (checkClipFile root) config.clips

checkClipFile :: Maybe FilePath -> ClipConfig -> Valid ()
checkClipFile mRoot config =
  checkFile (appendPath mRoot config.file)

withRoot ::
  (HasField "dir" a (Maybe FilePath)) =>
  a -> Maybe FilePath -> (Maybe FilePath -> Valid ()) -> Valid ()
withRoot config mRoot cont = do
  isOk <- checkDir root
  when isOk $ cont root
  where
    root = appendMaybePath mRoot config.dir

fileDoesNotExist :: FilePath -> Valid ()
fileDoesNotExist file =
  tell $ ["Error: file does not exist:\n    " <> Text.pack file]

directoryDoesNotExist :: FilePath -> Valid ()
directoryDoesNotExist file =
  tell $ ["Error: directory does not exist:\n    " <> Text.pack file]

-------------------------------------------------------------------------------------
-- volumes

checkVolumes :: Config -> Valid ()
checkVolumes config = do
  checkMixerVolumes config.mixer
  checkSamplerVolumes config.sampler
  mapM_ checkFxVolumes config.fxs

checkMixerVolumes :: MixerConfig -> Valid ()
checkMixerVolumes config = do
  checkMasterVolume config.master
  mapM_ checkChannelVolume config.channels
  where
    checkMasterVolume :: MasterConfig -> Valid ()
    checkMasterVolume master = do
      checkVolume master.volume
      mapM_ checkVolume master.gain

    checkChannelVolume :: ChannelConfig -> Valid ()
    checkChannelVolume channel = do
      checkVolume channel.volume
      mapM_ checkVolume channel.gain

checkSamplerVolumes :: SamplerConfig -> Valid ()
checkSamplerVolumes config = do
  mapM_ checkTrackVolume config.tracks
  mapM_ checkClipsVolume config.clips
  where
    checkTrackVolume :: TrackConfig -> Valid ()
    checkTrackVolume track = do
      mapM_ checkVolume track.gain
      mapM_ checkStemVolume track.stems

    checkClipsVolume :: ClipsConfig -> Valid ()
    checkClipsVolume clips = do
      mapM_ checkClipColumnVolume clips.columns

    checkStemVolume :: StemConfig -> Valid ()
    checkStemVolume stem = do
      mapM_ checkVolume stem.volume
      mapM_ checkVolume stem.gain

    checkClipColumnVolume :: ClipColumnConfig -> Valid ()
    checkClipColumnVolume column = do
      mapM_ checkVolume column.gain
      mapM_ checkClipVolume column.clips

    checkClipVolume :: ClipConfig -> Valid ()
    checkClipVolume clip =
      mapM_ checkVolume clip.gain

checkFxVolumes :: FxConfig -> Valid ()
checkFxVolumes config =
  case config.input of
    GroupFx fx -> mapM_ checkFxInput fx.inputChannels
    _ -> pure ()
  where
    checkFxInput :: FxChannelInput -> Valid ()
    checkFxInput input = checkVolume input.gain

checkVolume :: Float -> Valid ()
checkVolume vol =
  unless (vol >= 0 && vol <= maxAllowedVolume) $
    tell ["Error: volume out of range: " <> Text.pack (show vol)]

maxAllowedVolume :: Float
maxAllowedVolume = 10

-------------------------------------------------------------------------------------
-- channels

checkChannels :: Config -> Valid ()
checkChannels config = do
  checkSamplerChannels config.sampler
  mapM_ checkFxChannels config.fxs

checkSamplerChannels :: SamplerConfig -> Valid ()
checkSamplerChannels config = do
  mapM_ checkStem $ (.stems) =<< config.tracks
  mapM_ (uncurry checkClipColumn) $ zip [1..] $ (.columns) =<< maybeToList config.clips
  where
    checkStem :: StemConfig -> Valid ()
    checkStem stem =
      checkChannel ("stem " <> Text.pack stem.file) stem.channel

    checkClipColumn :: Int -> ClipColumnConfig -> Valid ()
    checkClipColumn index column = do
      mapM_ (checkChannel $ "Clip column at " <> Text.pack (show index)) column.channel
      mapM_ checkClip column.clips

    checkClip :: ClipConfig -> Valid ()
    checkClip clip = do
      mapM_ (checkChannel ("Clip " <> clip.name.name)) clip.channel

checkFxChannels :: FxConfig -> Valid ()
checkFxChannels config =
  case config.input of
    MasterFx -> pure ()
    ChannelFx (ChannelFxConfig channel) -> checkChannel "Fx channel" channel
    GroupFx (GroupFxConfig ins out) -> do
      mapM_ checkFxChannelInput ins
      checkChannel "Group Fx output" out
  where
    checkFxChannelInput :: FxChannelInput -> Valid ()
    checkFxChannelInput input =
      checkChannel "Group Fx input" input.channel

checkChannel :: Text -> Int -> Valid ()
checkChannel tag n =
  unless (n > 0) $ tell ["Error: channel should be positive: " <> tag]
