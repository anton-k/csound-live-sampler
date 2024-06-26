module Live.Config.Validate (
  checkConfig,
) where

import Control.Monad
import Control.Monad.Writer.Strict
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Records
import Live.Config.Types
import Live.Scene.Common (NameRef (..))
import Live.Scene.Mixer.Config
import Live.Scene.Sampler.Config
import System.Directory
import System.FilePath

{-| Checks that

* all files for stems exist
* volumes are within useful range
* audio and controllers are valid
* valid channels are used for stems
-}
checkConfig :: Config -> IO (Either Text Config)
checkConfig config = do
  mErr <- validateConfig config
  pure $ case mErr of
    Nothing -> Right config
    Just err -> Left err

validateConfig :: Config -> IO (Maybe Text)
validateConfig config = runValid $ do
  checkFiles config
  checkSampler config.sampler
  checkVolumes config
  checkAudio config
  checkControllers config
  checkChannels config
  where
    checkAudio _ = pure () -- TODO
    checkControllers _ = pure () -- TODO

checkSampler :: SamplerConfig NameRef -> Valid ()
checkSampler config =
  mapM_ (checkPlaylist config.tracks) config.playlist

checkPlaylist :: [TrackConfig NameRef] -> [Text] -> Valid ()
checkPlaylist tracks playlist =
  mapM_ checkName playlist
  where
    trackNames :: Set Text
    trackNames = Set.fromList $ fmap (.name) tracks

    checkName :: Text -> Valid ()
    checkName name =
      unless (Set.member name trackNames) $
        tell ["Playlist name not found in tracks: " <> name]

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
  pure $
    if null errs
      then Nothing
      else Just (Text.unlines $ fmap (<> "\n") errs)

checkSamplerFiles :: SamplerConfig NameRef -> Valid ()
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

checkTrack :: Maybe FilePath -> TrackConfig NameRef -> Valid ()
checkTrack mRoot config = do
  withRoot config mRoot $ \root ->
    mapM_ (checkStemFile root) config.stems

checkStemFile :: Maybe FilePath -> StemConfig NameRef -> Valid ()
checkStemFile mRoot config =
  checkFile (appendPath mRoot config.file)

appendMaybePath :: Maybe FilePath -> Maybe FilePath -> Maybe FilePath
appendMaybePath mA mB = case mA of
  Just a -> fmap (a </>) mB
  Nothing -> mB

appendPath :: Maybe FilePath -> FilePath -> FilePath
appendPath mPrefix = maybe id (</>) mPrefix

checkClips :: Maybe FilePath -> ClipsConfig NameRef -> Valid ()
checkClips mRoot config =
  withRoot config mRoot $ \root ->
    mapM_ (checkClipColumnFiles root) config.columns

checkClipColumnFiles :: Maybe FilePath -> ClipColumnConfig NameRef -> Valid ()
checkClipColumnFiles mRoot config = do
  withRoot config mRoot $ \root ->
    mapM_ (checkClipFile root) config.clips

checkClipFile :: Maybe FilePath -> ClipConfig NameRef -> Valid ()
checkClipFile mRoot config =
  checkFile (appendPath mRoot config.file)

withRoot ::
  (HasField "dir" a (Maybe FilePath)) =>
  a ->
  Maybe FilePath ->
  (Maybe FilePath -> Valid ()) ->
  Valid ()
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

checkMixerVolumes :: MixerConfig NameRef -> Valid ()
checkMixerVolumes config = do
  mapM_ checkMasterVolume config.master
  mapM_ checkChannelVolume config.channels
  where
    checkMasterVolume :: MasterConfig -> Valid ()
    checkMasterVolume master = do
      checkVolume master.volume
      mapM_ checkVolume master.gain

    checkChannelVolume :: ChannelConfig NameRef -> Valid ()
    checkChannelVolume channel = do
      checkVolume channel.volume
      mapM_ checkVolume channel.gain

checkSamplerVolumes :: SamplerConfig NameRef -> Valid ()
checkSamplerVolumes config = do
  mapM_ checkTrackVolume config.tracks
  mapM_ checkClipsVolume config.clips
  where
    checkTrackVolume :: TrackConfig NameRef -> Valid ()
    checkTrackVolume track = do
      mapM_ checkVolume track.gain
      mapM_ checkStemVolume track.stems

    checkClipsVolume :: ClipsConfig NameRef -> Valid ()
    checkClipsVolume clips = do
      mapM_ checkClipColumnVolume clips.columns

    checkStemVolume :: StemConfig NameRef -> Valid ()
    checkStemVolume stem = do
      mapM_ checkVolume stem.volume
      mapM_ checkVolume stem.gain

    checkClipColumnVolume :: ClipColumnConfig NameRef -> Valid ()
    checkClipColumnVolume column = do
      mapM_ checkVolume column.gain
      mapM_ checkClipVolume column.clips

    checkClipVolume :: ClipConfig NameRef -> Valid ()
    checkClipVolume clip =
      mapM_ checkVolume clip.gain

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

checkSamplerChannels :: SamplerConfig NameRef -> Valid ()
checkSamplerChannels config = do
  mapM_ checkStem $ (.stems) =<< config.tracks
  mapM_ (uncurry checkClipColumn) $ zip [1 ..] $ (.columns) =<< maybeToList config.clips
  where
    checkStem :: StemConfig NameRef -> Valid ()
    checkStem stem =
      checkChannel ("stem " <> Text.pack stem.file) stem.channel

    checkClipColumn :: Int -> ClipColumnConfig NameRef -> Valid ()
    checkClipColumn index column = do
      mapM_ (checkChannel $ "Clip column at " <> Text.pack (show index)) column.channel
      mapM_ checkClip column.clips

    checkClip :: ClipConfig NameRef -> Valid ()
    checkClip clip = do
      mapM_ (checkChannel ("Clip " <> clip.name.name)) clip.channel

checkChannel :: Text -> NameRef -> Valid ()
checkChannel tag = \case
  NameInt n -> unless (n > 0) $ tell ["Error: channel should be positive: " <> tag]
  NameRef _ -> pure ()
