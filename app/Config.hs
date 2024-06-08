module Config
  ( Args (..)
  , Command (..)
  , readArgs
  ) where

import Options.Applicative

data Args = Args
  { config :: FilePath
  , csdOutput :: Maybe FilePath
  }

data Command
  = Run
  | WriteCsd

readArgs :: IO (Command, Args)
readArgs = execParser opts
  where
    opts = info (liftA2 (,) parseCommand parseArgs <**> helper)
      ( fullDesc
     <> progDesc "create live performances"
     <> header "csound-live-sampler - app to create live performances launched with Csound" )

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
  [ commandBy "run" Run "run live performance"
  , commandBy "write-csd" WriteCsd "only generate csound file. Can be run with: csound my-performance.csd"
  ]
  where
  commandBy :: String -> Command -> String -> Mod CommandFields Command
  commandBy name script desc =
    command name (info (pure script) (progDesc desc))

parseArgs :: Parser Args
parseArgs =
  Args
    <$> strOption
        ( long "config"
        <> short 'c'
        <> metavar "YAML-file"
        <> help "Description of the performance" )
    <*> optional (strOption
        ( long "output"
        <> short 'o'
        <> help "Write CSD to file"
        <> metavar "CSD-file" ))
