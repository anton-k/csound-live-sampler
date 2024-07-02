module Scene
  ( SceneUi
  , initScene
  ) where

import Prelude
import Scene.Mixer
import Scene.Sampler
import Scene.Elem
import Scene.Html
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Action

type SceneUi =
  { mixer :: MixerUi
  , sampler :: SamplerUi
  }

type SetScene =
  { sampler :: SetSampler
  , mixer :: SetMixer
  }

initScene :: forall a b . SceneUi -> Scene -> Elem a b SetScene
initScene sceneUi sceneAct =
  { setup: do
      mixer <- mixer.setup
      sampler <- sampler.setup
      pure
        { sampler: sampler
        , mixer: mixer
        }

  , html:
      divId "root"
        [ HH.header [] [ HH.section_ [container [sampler.html] ]]
        , HH.main [HP.classes [HH.ClassName "container"]]
            [ mixer.html ]
        ]
  }
  where
    sampler = initSampler sceneUi.sampler sceneAct.sampler
    mixer = initMixer sceneUi.mixer sceneAct.mixer

