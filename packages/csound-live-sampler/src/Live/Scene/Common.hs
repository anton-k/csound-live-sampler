module Live.Scene.Common (
  ChannelId (..),
  toChannelId,
  SendId (..),
  AudioInputId (..),
  toAudioInputId,
  NameRef (..),
  lookupNameRef,
  NameMap (..),
  toNameMap,
  smoothControl,
) where

import Csound.Core (Sig, portk)
import Data.Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text

smoothControl :: Sig -> Sig
smoothControl controlSig = portk controlSig 0.01

newtype ChannelId = ChannelId {unChannelId :: Int}
  deriving (Show, Eq, Ord)

data SendId = SendId
  { from :: ChannelId
  , to :: ChannelId
  }
  deriving (Show, Eq, Ord)

toChannelId :: Int -> ChannelId
toChannelId n = ChannelId (n - 1)

newtype AudioInputId = AudioInputId {unAudioInputId :: Int}

toAudioInputId :: Int -> AudioInputId
toAudioInputId n = AudioInputId (n - 1)

data NameRef
  = NameRef Text
  | NameInt Int

instance ToJSON NameRef where
  toJSON = \case
    NameRef text -> toJSON text
    NameInt num -> toJSON num

instance FromJSON NameRef where
  parseJSON = \case
    Number n -> pure (NameInt $ floor n)
    String txt -> pure (NameRef txt)
    _ -> fail "Failed to parse NameRef: use integer or string"

newtype NameMap = NameMap (Map Text Int)

lookupNameRef :: NameRef -> NameMap -> Int
lookupNameRef ref (NameMap m) =
  case ref of
    NameRef name -> onError name $ Map.lookup name m
    NameInt n -> n
  where
    onError name = fromMaybe (error $ "Name not found: " <> Text.unpack name)

toNameMap :: [(Text, Int)] -> NameMap
toNameMap = NameMap . Map.fromList
