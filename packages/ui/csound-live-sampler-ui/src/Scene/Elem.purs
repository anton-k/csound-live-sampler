module Scene.Elem
  ( Elem
  , emptyElem
  ) where

import Prelude
import Effect (Effect)
import Halogen.HTML as HH

type Elem w i =
  { setup :: Effect Unit
  , html :: HH.HTML w i
  }

emptyElem :: forall a b . Elem a b
emptyElem =
  { setup: pure unit
  , html: HH.div_ []
  }
