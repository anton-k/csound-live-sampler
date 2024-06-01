module Live.Scene.Fx.Unit
  ( Unit (..)
  , FxParamName
  , ParamMap
  , Bpm (..)
  , readParam
  , newParamMap
  , dampParam
  , frequencyParam
  ) where

import Csound.Core
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Map.Strict qualified as Map
import Data.Maybe

type FxParamName = Text
type ParamMap = Map FxParamName (Ref Sig)

newtype Bpm = Bpm (SE Sig)

data Unit a = Unit
  { needsBpm :: Bool
  , getParams :: a -> SE ParamMap
  , apply :: Bpm -> ParamMap -> a -> Sig2 -> SE Sig2
  }

-- * utils

readParam :: ParamMap -> FxParamName -> SE Sig
readParam params name =
  readRef ref
  where
    ref = fromMaybe (error errMessage) $ Map.lookup name params

    errMessage = "No required param: " <> Text.unpack name

newParamMap :: forall config . config -> [(FxParamName, (config -> Float))] -> SE ParamMap
newParamMap config args =
  Map.fromList <$> mapM param args
  where
    param :: (FxParamName, (config -> Float)) -> SE (FxParamName, Ref Sig)
    param (name, extract) = do
      ref <- newCtrlRef $ float (extract config)
      pure (name, ref)

dampFrequencyRange :: Num a => (a, a)
dampFrequencyRange = (100, 14000)

dampParam :: Sig -> Sig
dampParam = frequencyParam dampFrequencyRange

frequencyParam :: (Sig, Sig) -> Sig -> Sig
frequencyParam (minVal, maxVal) param =
  scale (expcurve param 4) maxVal minVal
