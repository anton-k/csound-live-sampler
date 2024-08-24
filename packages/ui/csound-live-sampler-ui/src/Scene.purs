module Scene
  ( initScene
  , SetScene
  , module Scene.Config
  ) where

import Prelude
import Scene.Mixer
import Scene.Sampler
import Scene.AudioCard
import Scene.Elem
import Scene.Html
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Action
import Scene.Config

type SetScene =
  { sampler :: SetSampler
  , mixer :: SetMixer
  , audioCard :: SetAudioCard
  }

initScene :: forall a b . SceneUi -> Scene -> Elem a b SetScene
initScene sceneUi sceneAct =
  { setup: do
      mixer <- mixer.setup
      sampler <- sampler.setup
      audioCard <- audioCard.setup
      pure { sampler, mixer, audioCard }

  , html:
      divId "root"
        [ HH.header [] [ HH.section_ [container [sampler.html] ]]
        , HH.main [HP.classes [HH.ClassName "container"]]
            [ audioCard.addHtml mixer.html ]
        ]
  }
  where
    sampler = initSampler sceneUi.sampler sceneAct.sampler
    mixer = initMixer sceneUi.mixer sceneAct.mixer
    audioCard = initAudioCard sceneUi.audioCard
