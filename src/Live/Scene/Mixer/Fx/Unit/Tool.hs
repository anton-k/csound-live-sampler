module Live.Scene.Mixer.Fx.Unit.Tool
  ( toolUnit
  ) where

import Data.Boolean ((<=*), (==*))
import Data.Maybe
import Live.Scene.Mixer.Fx.Unit
import Live.Scene.Mixer.Fx.Config (ToolConfig (..))
import Csound.Core

toolUnit :: Unit ToolConfig
toolUnit = Unit
  { needsBpm = False
  , getParams = toolParams
  , apply = \_bpm params _config -> toolFx params
  }

toolParams :: ToolConfig -> SE ParamMap
toolParams config =
  newParamMap config
    [ ("volume", fromMaybe 1 . (.volume))
    , ("gain", fromMaybe 1 . (.gain))
    , ("pan", fromMaybe 0.5 . (.pan))
    , ("width", fromMaybe 0.5 . (.width))
    ]

toolFx :: ParamMap -> Sig2 -> SE Sig2
toolFx params ins = do
  volume <- param "volume"
  gain <- param "gain"
  pan <- dampParam <$> param "pan"
  width <- dampParam <$> param "width"
  pure $ stereoWidth width $ stereoPan pan $ mul (volume * gain) ins
  where
    param = readParam params

stereoPan :: Sig -> Sig2 -> Sig2
stereoPan k (left, right) =
  ifB (k ==* 0.5)
    (left, right)
    (ifB (k <=* 0.5)
      (left, right * 2 * k)
      (left * 2 * (k - 0.5), right))

stereoWidth :: Sig -> Sig2 -> Sig2
stereoWidth k (left, right) =
  ifB (k ==* 0.5)
    (left, right)
    (left2, right2)
  where
    mid = (left + right) / float sqrt2
    side = (left - right) / float sqrt2

    mid2 = 2 * (1 - k) * mid
    side2 = 2 * k * side

    left2 = (mid2 + side2) / float sqrt2
    right2 = (mid2 - side2) / float sqrt2

sqrt2 :: Float
sqrt2 = sqrt 2


