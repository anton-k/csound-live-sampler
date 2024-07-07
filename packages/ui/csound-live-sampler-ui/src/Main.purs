module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Effect.Class (liftEffect)
import Halogen.Hooks as Hooks
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff, runAff_)
import Scene (SceneUi, initScene)
import Scene.Config (sceneUiFromJson)
import Scene.Config.Const (sceneUi, oscConfig) as Const
import Osc.Client (OscClient, newOscClient, newOscPort, readUiInfo)
import Osc.Message
import Network.Osc as Osc
import Effect.Console (log)
import Effect.Aff (Milliseconds(..), delay, launchAff_, Canceler, makeAff, nonCanceler)
import Data.Either
import JSON as J

main :: Effect Unit
main = do
  oscPort <- newOscPort Const.oscConfig
  withUiInfoExample (runApp oscPort)

withUiInfoExample :: (SceneUi -> Effect Unit) -> Effect Unit
withUiInfoExample cont = cont Const.sceneUi

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
  let
    ui = initScene sceneUi sceneAct.send
  Hooks.useLifecycleEffect do
    liftEffect $ do
      setter <- ui.setup
      sceneAct.listen.bpm setter.sampler.setBpm
      sceneAct.listen.channelVolumeEnvelope
        (\chanId val -> (setter.mixer.setChannel chanId).setVolumeEnvelope val)
      sceneAct.listen.channelVolume
        (\chanId val -> (setter.mixer.setChannel chanId).setVolume val)
      sceneAct.listen.channelMute
        (\chanId val -> (setter.mixer.setChannel chanId).setMute val)
      sceneAct.listen.partChange setter.sampler.setPart
      sceneAct.send.info.getCurrentPart

    pure Nothing
  let
    view =  ui.html
  Hooks.pure view
