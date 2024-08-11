module Live.Scene.Mixer.Fx.Unit.Tool (
  toolUnit,
) where

import Csound.Core
import Data.Boolean ((<=*), (==*))
import Data.Map.Strict qualified as Map
import Data.Maybe
import Live.Scene.Mixer.Fx.Config (ToolConfig (..))
import Live.Scene.Mixer.Fx.Unit

toolUnit :: Unit ToolConfig
toolUnit =
  Unit
    { needsBpm = False
    , getName = (.name)
    , getParams = toolParams
    , apply = \_bpm params _config -> toolFx params
    }

toolParams :: ToolConfig -> ParamNameInitMap
toolParams config =
  Map.fromList
    [ ("volume", fromMaybe 1 config.volume)
    , ("gain", fromMaybe 0.5 config.gain)
    , ("pan", fromMaybe 0.5 config.pan)
    , ("width", fromMaybe 0.5 config.width)
    ]

toolFx :: ParamMap -> Sig2 -> SE Sig2
toolFx params ins = do
  volume <- param "volume"
  gain <- param "gain"
  pan <- param "pan"
  width <- param "width"
  pure $ stereoWidth pan width $ {- stereoPan pan $ -} mul (volume * 2 * gain) ins
  where
    param = readParam params

stereoPan :: Sig -> Sig2 -> Sig2
stereoPan k (left, right) =
  ifB
    (k ==* 0.5)
    (left, right)
    ( ifB
        (k <=* 0.5)
        (left, right * 2 * k)
        (left * 2 * (k - 0.5), right)
    )

stereoWidth :: Sig -> Sig -> Sig2 -> Sig2
stereoWidth panValue widthValue (left, right) =
  (0.5 * (1 + panNorm) * mid + 2 * widthValue * side, 0.5 * (1 - panNorm) * mid - 2 * widthValue * side)
  where
    mid = (left + right) / 2
    side = (left - right) / 2

    -- converts from [0, 1] to [-1, 1]
    panNorm = 2 * (panValue - 0.5)

{-
stereoWidth :: Sig -> Sig2 -> Sig2
stereoWidth k (left, right) =
  ifB
    (k ==* 0.5)
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
-}
