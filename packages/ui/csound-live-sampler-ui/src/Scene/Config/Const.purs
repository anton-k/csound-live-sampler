module Scene.Config.Const
  ( sceneUi
  , oscConfig
  ) where

import Prelude
import Scene (SceneUi)
import Data.Array (range)
import Data.Maybe (Maybe(..))
import Osc.Client (OscConfig)

oscConfig :: OscConfig
oscConfig =
  { address: "ws://localhost"
  , port: 9090
  }

sceneUi :: SceneUi
sceneUi =
  { mixer:
      { channels: map (\n -> { channel: n, volume: 0.5, fxs: [], name: Nothing }) (range 1 8)
      , master: { volume: 1.0 , fxs: [] }
      }

  , sampler:
      { tracks:
          [ track1
          , track2
          ]
      }
  }
  where
    track1 =
      { name: "Never Enough"
      , size: 4
      }

    track2 =
      { name: "Samurai"
      , size: 4
      }
