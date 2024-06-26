module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
-- import Effect.Console (log)
import Effect.Class (liftEffect)
import Halogen.Hooks as Hooks
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Scene (initScene)
import Scene.Config (scene)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    void $ runUI hookComponent Nothing body

hookComponent
  :: forall unusedQuery unusedInput unusedOutput
   . H.Component unusedQuery unusedInput unusedOutput Aff
hookComponent = Hooks.component \_ _ -> Hooks.do
  enabled /\ enabledIdx <- Hooks.useState false
  let label = if enabled then "On" else "Off"
      ui = initScene scene
  Hooks.useLifecycleEffect do
    liftEffect $ ui.setup

    pure Nothing
  let
    button =
        HH.button
          [ HP.title label
          , HE.onClick \_ -> Hooks.modify_ enabledIdx not
          ]
          [ HH.text label ]

    view =
      HH.div_
        [ ui.html
        , button
        ]
  Hooks.pure view


{-
HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type State = Int

data Action = Increment | Decrement

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ = 0

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.header_ [HH.h2_ [HH.text "Increment me!"]]
    , HH.main [HP.classes [ HH.ClassName "container" ]]
        [ HH.div [HP.classes [HH.ClassName "row"]]
          (map  (\x -> HH.div [HP.classes [HH.ClassName "col-md-3"]] [x])
          [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
          , HH.text (show state)
          , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
          ])
        ]
    ]

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Decrement ->
    H.modify_ \state -> state - 1

  Increment ->
    H.modify_ \state -> state + 1
  -}
