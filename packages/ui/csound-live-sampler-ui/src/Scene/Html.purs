module Scene.Html
  ( divClasses
  , divId
  , container
  , row
  , col
  , textButton
  , accordion
  , filledGrid
  ) where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Common.Array (fillRow)

divClasses cs a = HH.div [HP.classes (map HH.ClassName cs)] a
divId name a = HH.div [HP.id name] a

container a = divClasses ["container"] a
row a = divClasses ["row"] a
col n a = divClasses ["col-" <> show n] a

textButton label =
    HH.button
      [ HP.title label
      -- , HE.onClick \_ -> Hooks.pure unit
      ]
      [ HH.text label ]

accordion :: forall w i. String -> HH.HTML w i -> HH.HTML w i
accordion title body =
  HH.details []
    [ HH.summary [HP.classes [HH.ClassName "outline", HH.ClassName "constrast"]] [HH.text title]
    , body
    ]

filledGrid :: forall w i. Int -> Array (HH.HTML w i) -> HH.HTML w i
filledGrid size items =
  divClasses ["grid"] (fillRow size emptyHtml items)
  where
    emptyHtml = HH.div [] []
