module Live.Config.Validate
  ( validateConfig
  ) where

import Control.Monad
import Control.Monad.Writer.Strict
import System.Directory
import System.FilePath
import Live.Config.Types
import Data.Text (Text)
import Data.Text qualified as Text
import Live.Scene.Sampler.Config
  (SamplerConfig (..), StemConfig (..), TrackConfig (..),
   ClipsConfig (..), ClipColumnConfig (..), ClipConfig (..),
  )
import Data.Foldable (asum)
import GHC.Records

-- | Nothing if everythong is ok
--
-- Checks that
--
-- * all files for stems exist
-- * volumes are within useful range
-- * audio and controllers are valid
-- * valid channels are used for stems
validateConfig :: Config -> IO (Maybe Text)
validateConfig config = do
  files <- checkFiles config
  vols <- checkVolumes config
  audio <- checkAudio config
  controls <- checkControllers config
  chans <- checkChannels config
  pure $ asum [files, vols, audio, controls, chans]
  where
    checkVolumes _ = pure Nothing -- TODO
    checkAudio _ = pure Nothing -- TODO
    checkControllers _ = pure Nothing -- TODO
    checkChannels _ = pure Nothing -- TODO

checkFiles :: Config -> IO (Maybe Text)
checkFiles config = do
  errs <- checkSamplerFiles config.sampler
  pure $ if null errs
    then Nothing
    else Just (Text.unlines errs)

type Valid a = WriterT [Text] IO a

checkSamplerFiles :: SamplerConfig -> IO [Text]
checkSamplerFiles config = fmap snd $ runWriterT $ do
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
    mapM_ (checkClipColumn root) config.columns

checkClipColumn :: Maybe FilePath -> ClipColumnConfig -> Valid ()
checkClipColumn mRoot config = do
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
