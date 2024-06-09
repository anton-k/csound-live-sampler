module Live.Scene.Audio.Config
  ( AudioConfig (..)
  , AudioInputConfig (..)
  , MonoInputConfig (..)
  , StereoInputConfig (..)
  , AudioOutputConfig (..)
  , MonoOutputConfig (..)
  , StereoOutputConfig (..)
  ) where

import Data.Text (Text)
import Data.Aeson.TH qualified as Json
import Data.Default

data AudioConfig channel = AudioConfig
  { csound :: Maybe Text
  , inputs :: Maybe [AudioInputConfig channel]
  , outputs :: Maybe [AudioOutputConfig channel]
  }
  deriving (Functor)

instance Default (AudioConfig a) where
  def = AudioConfig { csound = Nothing, inputs = Nothing, outputs = Nothing }

data AudioInputConfig channel
  = StereoAudioInputConfig (StereoInputConfig channel)
  | MonoAudioInputConfig (MonoInputConfig channel)
  deriving (Functor)

data StereoInputConfig channel = StereoInputConfig
  { name :: Maybe Text
  , channel :: channel
  , gain :: Maybe Float
  , stereo :: (Int, Int)
  }
  deriving (Functor)

data MonoInputConfig channel = MonoInputConfig
  { name :: Maybe Text
  , channel :: channel
  , gain :: Maybe Float
  , mono :: Int
  }
  deriving (Functor)

data AudioOutputConfig channel
  = StereoAudioOutputConfig (StereoOutputConfig channel)
  | MonoAudioOutputConfig (MonoOutputConfig channel)
  deriving (Functor)

data StereoOutputConfig channel = StereoOutputConfig
  { channel :: Maybe channel
  , gain :: Maybe Float
  , stereo :: Maybe (Int, Int)
  }
  deriving (Functor)

data MonoOutputConfig channel = MonoOutputConfig
  { channel :: Maybe channel
  , gain :: Maybe Float
  , mono :: Int
  }
  deriving (Functor)

$(Json.deriveJSON Json.defaultOptions ''MonoInputConfig)
$(Json.deriveJSON Json.defaultOptions ''StereoInputConfig)
$(Json.deriveJSON Json.defaultOptions { Json.sumEncoding = Json.UntaggedValue } ''AudioInputConfig)
$(Json.deriveJSON Json.defaultOptions ''MonoOutputConfig)
$(Json.deriveJSON Json.defaultOptions ''StereoOutputConfig)
$(Json.deriveJSON Json.defaultOptions { Json.sumEncoding = Json.UntaggedValue } ''AudioOutputConfig)
$(Json.deriveJSON Json.defaultOptions ''AudioConfig)
