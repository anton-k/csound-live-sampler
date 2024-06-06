module Main (main) where

import Config qualified as Config
import Live.Config
import Live.Scene
import Data.Text.IO qualified as Text

main :: IO ()
main = do
  (command, args) <- Config.readArgs
  eConfig <- readConfig args.config
  case eConfig of
    Right config -> do
      case command of
        Config.Run -> runScene config args.csdOutput
        Config.WriteCsd -> writeSceneCsd config args.csdOutput
    Left err -> Text.putStrLn err
