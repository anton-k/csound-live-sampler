module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Effect.Class (liftEffect)
import Halogen.Hooks as Hooks
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Scene (SceneUi, initScene, SetScene)
import Scene.Config.Const (sceneUi, oscConfig) as Const
import Osc.Client (OscClient, newOscClient, newOscPort, readUiInfo)
import Network.Osc as Osc
import Effect.Console (log)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Data.Either (Either (..))

main :: Effect Unit
main = do
  oscPort <- newOscPort Const.oscConfig
  runApp oscPort Const.sceneUi

runApp :: Osc.Port -> SceneUi -> Effect Unit
runApp oscPort sceneUiExample = do
  sceneAct <- newOscClient oscPort
  HA.runHalogenAff do
    body <- HA.awaitBody
    delay $ Milliseconds 100.0
    eSceneUi <- readUiInfo oscPort
    sceneUi <- liftEffect $ case eSceneUi of
      Right sceneUi -> pure sceneUi
      Left err -> do
        log ("Read UI info error: " <> err)
        pure sceneUiExample

    void $ runUI (hookComponent sceneUi sceneAct) Nothing body

hookComponent
  :: forall unusedQuery unusedInput unusedOutput
   . SceneUi -> OscClient -> H.Component unusedQuery unusedInput unusedOutput Aff
hookComponent sceneUi sceneAct = Hooks.component \_ _ -> Hooks.do
  _enabled /\ _enabledIdx <- Hooks.useState false
  Hooks.useLifecycleEffect do
    liftEffect $ do
      setter <- ui.setup
      linkSettersToOsc sceneAct setter
      sceneAct.send.info.getCurrentPart
    pure Nothing
  Hooks.pure ui.html
  where
    ui = initScene sceneUi sceneAct.send

linkSettersToOsc :: OscClient -> SetScene -> Effect Unit
linkSettersToOsc sceneAct setter = do
  sceneAct.listen.bpm setter.sampler.setBpm
  sceneAct.listen.masterVolumeEnvelope setter.mixer.setMaster.setVolumeEnvelope
  sceneAct.listen.masterVolume setter.mixer.setMaster.setVolume
  sceneAct.listen.channelVolumeEnvelope
    (\chanId val -> (setter.mixer.setChannel chanId).setVolumeEnvelope val)
  sceneAct.listen.channelVolume
    (\chanId val -> (setter.mixer.setChannel chanId).setVolume val)
  sceneAct.listen.channelMute
    (\chanId val -> (setter.mixer.setChannel chanId).setMute val)
  sceneAct.listen.fxParam setter.mixer.setFxParam
  sceneAct.listen.sendFx setter.mixer.setSendFx
  sceneAct.listen.fxBypass setter.mixer.setFxBypass
  sceneAct.listen.partChange setter.sampler.setPart
