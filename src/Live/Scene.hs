module Live.Scene
  ( Scene
  , runScene
  , loadScene
  , TrackId
  , PartId
  , Clip
  , Act (..)
  , act
  ) where

import Live.Config
import Csound.Core

runScene :: SE Scene -> IO ()
runScene = undefined

data Scene

-- * Mixer

setVolume :: Scene -> Sig -> SE ()
setVolume = undefined

setChanVolume :: Scene -> D -> Sig -> SE ()
setChanVolume = undefined

-- * Parts

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

-- | Messages
data Act
  -- v1
  = SetMasterVolume D
  | SetChanVolume ChanId D
  | ToggleMute ChanId
  | NextClip Clip
  -- v2
  | SetFxSend ChanId FxId D

act :: Scene -> Act -> SE ()
act = undefined

loadScene :: Config -> SE Scene
loadScene = undefined
