module Scene.Config
  ( scene
  ) where

import Prelude
import Scene (Scene)
import Data.Array (range)
import Data.Maybe (Maybe(..))

scene :: Scene
scene =
  { mixer:
      { items: map (\n -> { channel: n, volume: 0.5, fxs: [] }) (range 1 8)
      }

  , sampler:
      { tracks:
          [ track1
          , track2
          , track3
          ]
      , current: Nothing
      , bpm: 120.0
      , measure: 4
      }
  }
  where
    track1 =
      { name: "foo"
      , size: 4
      , current: 0
      }

    track2 =
      { name: "bar"
      , size: 4
      , current: 0
      }

    track3 =
      { name: "baz"
      , size: 6
      , current: 0
      }

