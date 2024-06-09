module Live.Scene.Common.Aeson
  ( dropPrefix
  , headToLower
  ) where

import Data.Char qualified as Char
import Data.List qualified as List

dropPrefix :: String -> String -> String
dropPrefix prefix str
  | List.isPrefixOf prefix str = headToLower $ drop 3 str
  | otherwise = headToLower str

headToLower :: String -> String
headToLower = \case
  [] -> []
  x : xs  -> Char.toLower x : xs



