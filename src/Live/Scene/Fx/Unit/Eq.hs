module Live.Scene.Fx.Unit.Eq
  ( eqUnit
  , mixerEqUnit
  ) where

import Live.Scene.Fx.Unit
import Live.Scene.Fx.Config (EqConfig (..), MixerEqConfig (..), EqPoint (..), EqMode (..))
import Csound.Core hiding (mode)
import Data.Text qualified as Text
import Data.List qualified as List
import Data.Monoid (Endo (..))
import Control.Monad
import Data.Maybe

-- * Parametric EQ

eqUnit :: Unit EqConfig
eqUnit =
  Unit
    { needsBpm = False
    , getParams = eqParams
    , apply = \_bpm params config -> eqFx params config
    }

eqParams :: EqConfig -> SE ParamMap
eqParams config =
  newParamMap config $ eqPointParams =<< (take (length config.points) [0..])
  where
    eqPointParams :: Int -> [(FxParamName, (EqConfig -> Float))]
    eqPointParams index =
      [ (nameIndex "frequency", \cfg -> (cfg.points !! index).frequency)
      , (nameIndex "gain", \cfg -> (cfg.points !! index).gain)
      , (nameIndex "width", \cfg -> fromMaybe 0 (cfg.points !! index).width)
      ]
      where
        nameIndex name = name <> Text.pack (show (index + 1))

eqFx :: ParamMap -> EqConfig -> Sig2 -> SE Sig2
eqFx params config inputs = do
  args <- zipWithM (\index point -> readEqPointSig point.mode params index) [1..] config.points
  pure $ at (applyEqs args) inputs

-- * Mixer EQ

mixerEqUnit :: Unit MixerEqConfig
mixerEqUnit =
  Unit
    { needsBpm = False
    , getParams = mixerEqParams
    , apply = \_bpm params config -> mixerEqFx params config
    }

mixerEqParams :: MixerEqConfig -> SE ParamMap
mixerEqParams config =
  newParamMap config $ fmap toParams (take (length config.gains) [0..])
  where
    toParams :: Int -> (FxParamName, (MixerEqConfig -> Float))
    toParams index =
      (nameIndex "gain", \cfg -> cfg.gains !! index)
      where
        nameIndex name = name <> Text.pack (show (index + 1))

data EqArgs = EqArgs
  { mode :: EqMode
  , frequency :: Sig
  , gain :: Sig
  , width :: Sig
  }

readEqPointSig :: EqMode -> ParamMap -> Int -> SE EqArgs
readEqPointSig mode params index = do
  frequency <- param "frequency"
  gain <- param "gain"
  width <- param "width"
  pure EqArgs { frequency, gain, width, mode }
  where
    param name = readParam params (name <> Text.pack (show index))

applyEq :: EqArgs -> Sig -> Sig
applyEq = undefined

mixerEqFx :: ParamMap -> MixerEqConfig -> Sig2 -> SE Sig2
mixerEqFx params config input = do
  gains <- mapM (param "gain") [1..size]
  let
    args = List.zipWith4
      (\frequency width gain mode ->
          EqArgs
            { frequency = float frequency
            , width = float width
            , gain
            , mode
            }
      ) frequencies widths gains modes
  pure $ at (applyEqs args) input
  where
    size = length config.gains

    frequencies = mixerEqFrequencies size

    widths = mixerEqWidths size

    modes = mixerEqModes size

    param name index = readParam params (name <> Text.pack (show index))

applyEqs :: [EqArgs] -> Sig -> Sig
applyEqs args =
  appEndo $ foldMap (Endo . applyEq) args

mixerEqFrequencies :: Int -> [Float]
mixerEqFrequencies = undefined

mixerEqWidths :: Int -> [Float]
mixerEqWidths = undefined

-- | Produces shelves on the sides and band passes inside the interval
mixerEqModes :: Int -> [EqMode]
mixerEqModes size =
  case size of
    n | n <= 0 -> []
    1 -> [BandPassEq]
    _ -> LowShelfEq : replicate (size - 2) BandPassEq <> [HighPassEq]
