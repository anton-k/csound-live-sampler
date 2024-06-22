module Csound.Live.Sampler.Osc.Client (
  OscConfig (..),
  OscClient (..),
  newOscClient,
) where

import Control.Monad
import Network.Socket
import Network.Socket.ByteString as SB
import Vivid.OSC

data OscConfig = OscConfig
  { address :: String
  , port :: Int
  }

data OscClient = OscClient
  { send :: OSC -> IO ()
  }

newOscClient :: OscConfig -> IO OscClient
newOscClient config = do
  (addr : _) <- getAddrInfo Nothing (Just config.address) (Just $ show config.port)
  s <- socket (addrFamily addr) Datagram defaultProtocol
  connect s (addrAddress addr)
  pure $
    OscClient
      { send = \msg -> void $ SB.send s $ encodeOSC msg
      }
