module Main (main) where

import Live.Config
import Live.Scene
import System.Environment
import Data.Yaml qualified as Yaml
import Data.ByteString qualified as BS

main :: IO ()
main = do
  configFile : _ <- getArgs
  config <- readConfig configFile
  BS.putStr $ Yaml.encode config
  runScene (loadScene config)
