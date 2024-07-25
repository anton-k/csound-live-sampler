module Live.Scene.Common.Aeson (
  dropPrefix,
  dropSuffix,
  headToLower,
) where

import Data.Char qualified as Char
import Data.List qualified as List

dropPrefix :: String -> String -> String
dropPrefix prefix str
  | List.isPrefixOf prefix str = headToLower $ drop (length prefix) str
  | otherwise = headToLower str

dropSuffix :: String -> String -> String
dropSuffix suffix str
  | List.isSuffixOf suffix str = headToLower $ reverse $ drop (length suffix) (reverse str)
  | otherwise = headToLower str

headToLower :: String -> String
headToLower = \case
  [] -> []
  x : xs -> Char.toLower x : xs
