module Main (main) where

import Live.Config
import Live.Scene
import System.Environment
import Data.Yaml qualified as Yaml
import Data.ByteString qualified as BS
import Data.Text.IO qualified as Text

main :: IO ()
main = do
  configFile : _ <- getArgs
  eConfig <- readConfig configFile
  case eConfig of
    Right config -> do
      BS.putStr $ Yaml.encode config
      runScene config
    Left err -> Text.putStrLn err
