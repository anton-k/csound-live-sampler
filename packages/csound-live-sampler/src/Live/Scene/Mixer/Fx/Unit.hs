module Live.Scene.Mixer.Fx.Unit (
  Unit (..),
  FxParamName,
  ParamMap,
  ParamNameInitMap,
  Bpm (..),
  readParam,
  newParamMap,
  dampParam,
  frequencyParam,
) where

import Csound.Core
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Live.Scene.Common (smoothControl)

type FxParamName = Text
type ParamMap = Map FxParamName (Ref Sig)
type ParamNameInitMap = Map FxParamName Float

newtype Bpm = Bpm (SE Sig)

data Unit a = Unit
  { needsBpm :: Bool
  , getParams :: a -> ParamNameInitMap
  , getName :: a -> Text
  , apply :: Bpm -> ParamMap -> a -> Sig2 -> SE Sig2
  }

-- * utils

readParam :: ParamMap -> FxParamName -> SE Sig
readParam params name =
  smoothControl <$> readRef ref
  where
    ref = fromMaybe (error errMessage) $ Map.lookup name params

    errMessage = "No required param: " <> Text.unpack name

newParamMap :: ParamNameInitMap -> SE ParamMap
newParamMap args =
  mapM (newCtrlRef . float) args

dampFrequencyRange :: (Num a) => (a, a)
dampFrequencyRange = (100, 14000)

dampParam :: Sig -> Sig
dampParam = frequencyParam dampFrequencyRange

frequencyParam :: (Sig, Sig) -> Sig -> Sig
frequencyParam (minVal, maxVal) param =
  scale (expcurve param 4) maxVal minVal
