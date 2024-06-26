module Scene.Html
  ( divClasses
  , divId
  , container
  , row
  , col
  , textButton
  ) where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

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
