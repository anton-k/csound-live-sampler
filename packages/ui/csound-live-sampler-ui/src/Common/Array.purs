module Common.Array
  ( chunks
  , fillRow
  ) where

import Prelude
import Data.Array as Array

chunks :: forall a. Int -> Array a -> Array (Array a)
chunks _ [] = []
chunks n xs = pure (Array.take n xs) <> (chunks n $ Array.drop n xs)

fillRow :: forall a. Int -> a -> Array a -> Array a
fillRow maxChannelsPerRow fillValue xs =
  xs <> Array.replicate (maxChannelsPerRow - Array.length xs) fillValue

