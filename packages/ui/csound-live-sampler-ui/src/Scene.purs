module Scene
  ( Scene
  , initScene
  ) where

import Prelude
import Scene.Mixer
import Scene.Sampler
import Scene.Elem
import Scene.Html
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Scene =
  { mixer :: MixerUi
  , sampler :: SamplerUi
  }

initScene :: forall a b . Scene -> Elem a b
initScene scene =
  { setup: do
      mixer.setup
      sampler.setup

  , html:
      divId "root"
        [ HH.header [] [ HH.section_ [container [sampler.html] ]]
        , HH.main [HP.classes [HH.ClassName "container"]]
            [ mixer.html ]
        ]
  }
  where
    sampler = initSampler scene.sampler
    mixer = initMixer scene.mixer

