module Scene.Mixer
  ( MixerUi
  , MixerUiItem
  , initMixer
  , initChannel
  , Fx
  , FxParam
  ) where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Nexus.Ui.Core.Common as Ui
import Nexus.Ui.Core.Dial as Ui
import Nexus.Ui.General.Multislider as Ui
import Effect.Console (log)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Hooks as Hooks
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Control.Monad.Trans.Class (class MonadTrans)
import Effect.Aff (Aff)
import Data.Traversable (traverse_)
import Data.Array (range)
import Scene.Elem
import Scene.Html
import Data.Array as Array
import Action

type MixerUi =
  { items :: Array MixerUiItem
  }

type MixerUiItem =
  { channel :: Int
  , volume :: Number
  , fxs :: Array Fx
  , name :: Maybe String
  }

type Fx =
  { name :: String
  , params :: Array FxParam
  }

type FxParam =
  { name :: String
  , value :: Number
  }

initMixer :: forall w s . MixerUi -> Mixer -> Elem w s
initMixer mixer act =
  { setup: traverse_ (_.setup) items
  , html:
      divClasses ["grid"] (map (\item -> toColumn item.html) items)
  }
  where
    items = map (initChannel act) mixer.items

    toColumn x = divClasses [] [x]

initChannel :: forall w s . Mixer -> MixerUiItem -> Elem w s
initChannel act item =
  { setup: do
      dial <- Ui.newDial ("#" <> dialTarget)
      dial.setValue item.volume
      bar <- initBar ("#" <> barTarget) item.channel item.volume
      dial.on Ui.Change (\val -> do
          act.setChannelVolume item.channel val
          bar.setAllSliders [val]
        )

  , html:
      divClasses []
        [ divId barTarget []
        , divId "space1" [HH.p_ [HH.text " "]]
        , divId dialTarget []
        , divId "space1" [HH.p_ [HH.text " "]]
        , case item.name of
              Just name -> divClasses [] [HH.text name]
              Nothing -> divClasses [] []
        , if not (Array.null item.fxs) then button else divClasses [] []
        ]
  }
  where
    barTarget = "bar" <> show item.channel
    dialTarget = "dial" <> show item.channel
    button = textButton "FX"

initBar :: String -> Int -> Number -> Effect Ui.Multislider
initBar target n initValue = do
  bar <- Ui.newMultisliderBy target
    { size: 60.0 /\ 300.0
    , min: 0.0
    , max: 1.0
    , step: 0.0
    , values: [initValue]
    , candycane: 3.0
    , smoothing: 0.0
    , mode: Ui.BarMultislider
    , numberOfSliders: 1
    }
  bar.on Ui.Change (\val -> log ("Slider" <> show n <> ": " <> show val))
  pure bar
