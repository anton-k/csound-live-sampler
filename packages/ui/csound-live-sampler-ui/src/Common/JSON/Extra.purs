-- move me to a library
module Common.JSON.Extra
  ( lookupString
  , lookupNumber
  , lookupInt
  , lookupBoolean
  , lookupArray
  , lookupJObject
  ) where

import Prelude
import JSON (JSON)
import JSON as Json
import JSON.Object as Json
import Data.Int (round)
import Data.Maybe (Maybe(..))

lookupString :: String -> Json.JObject -> Maybe String
lookupString name obj =
  Json.toString =<< Json.lookup name obj

lookupNumber :: String -> Json.JObject -> Maybe Number
lookupNumber name obj =
  Json.toNumber =<< Json.lookup name obj

lookupInt :: String -> Json.JObject -> Maybe Int
lookupInt name obj = map round $ lookupNumber name obj

lookupBoolean :: String -> Json.JObject -> Maybe Boolean
lookupBoolean name obj =
  Json.toBoolean =<< Json.lookup name obj

lookupArray :: String -> Json.JObject -> Maybe (Array JSON)
lookupArray name obj =
  Json.toArray =<< Json.lookup name obj

lookupJObject :: String -> Json.JObject -> Maybe Json.JObject
lookupJObject name obj =
  Json.toJObject =<< Json.lookup name obj
