module Scene.Config
  ( sceneUi
  , oscConfig
  ) where

import Prelude
import Scene (SceneUi)
import Data.Array (range)
import Data.Maybe (Maybe(..))
import Osc.Client

oscConfig :: OscConfig
oscConfig =
  { address: "127.0.0.1"
  , port: 12400
  }

sceneUi :: SceneUi
sceneUi =
  { mixer:
      { items: map (\n -> { channel: n, volume: 0.5, fxs: [], name: Nothing }) (range 1 8)
      }

  , sampler:
      { tracks:
          [ track1
          , track2
          , track3
          , track4
          ]
      , current: Nothing
      , bpm: 120.0
      , measure: 4
      }
  }
  where
    track1 =
      { name: "Never Enough"
      , size: 4
      , current: 0
      }

    track2 =
      { name: "Samurai"
      , size: 4
      , current: 0
      }

    track3 =
      { name: "La melody"
      , size: 6
      , current: 0
      }

    track4 =
      { name: "Big stones"
      , size: 6
      , current: 0
      }

