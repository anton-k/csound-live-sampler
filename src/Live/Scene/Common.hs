module Live.Scene.Common
  ( ChannelId (..)
  , toChannelId
  ) where

newtype ChannelId = ChannelId { unChannelId :: Int }
  deriving (Show, Eq, Ord)

toChannelId :: Int -> ChannelId
toChannelId n = ChannelId (n - 1)

