module Scene.Sampler
  ( SetSampler
  , initSampler
  , module Scene.Sampler.Config
  ) where

import Prelude
import Data.Tuple (Tuple (..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Scene.Elem
import Scene.Html
import Data.Maybe
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Data.Array as Array
import DOM.HTML.Indexed.InputType (InputType (..))
import Data.Foldable
import Nexus.Ui.General.RadioButton
import Nexus.Ui.General.TextButton
import Nexus.Ui.General.Select
import Nexus.Ui.Core.Dial
import Nexus.Ui.Core.Common
import Data.Int (toNumber, round)
import Action
import Effect.Console
import Osc.Client (Clip (..))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Scene.Sampler.Config (SamplerUi, TrackUi)
import Osc.Client (Control, newOscControl)

loopColor = "#24bcbc"
playNextColor = "#001f3f"
stopColor = "#FF4136"
partLabelColor = "#3D9970"
partLabelLastPartColor = "#FF851B"

type SetSampler =
  { setBpm :: Int -> Effect Unit
  , setPart :: Clip -> Effect Unit
  }

initSampler :: forall a b . SamplerUi -> Sampler -> Elem a b SetSampler
initSampler sampler act =
  { setup: do
      bpm <- newRadioButtonBy "#bpm"
        { size: (25.0 * 1.15 * toNumber defaultMeasure) /\ 25.0
        , numberOfButtons: defaultMeasure
        , active: 0
        }
      prevTrack <- navButton "#prevTrack" "<<="
      prevTrack.on Change $ onButtonOn $ act.shiftTrack (-1)
      nextTrack <- navButton "#nextTrack" "=>>"
      nextTrack.on Change $ onButtonOn $ act.shiftTrack 1

      prevPart <- navButton "#prevPart" "<<"
      prevPart.on Change $ onButtonOn $ act.shiftPart (-1)
      nextPart <- navButton "#nextPart" ">>"
      nextPart.on Change $ onButtonOn $ act.shiftPart 1
      partLabel <- navDial "#partLabel"
      partLabel.colorize AccentColor "#3D9970"

      trackList <- navSelect "#trackList" (map (_.name) sampler.tracks)
      sendOscTrack <- newOscControl $ \item ->
        act.setTrack (item.index + 1)
      trackList.on Change sendOscTrack.set
      loopDial <- navDial "#loopDial"
      loopCounter <- initLoopCounter 16

      pure
        { setBpm: setBpm bpm loopCounter loopDial
        , setPart: updatePart bpm trackList sendOscTrack partLabel loopCounter loopDial
        }

  , html:
      HH.nav_ $ map (\items -> HH.ul_ (map (HH.li_ <<< pure) items))
        [ [bpmLights]
        , [ divId "loopDial" []
          , divId "trackList" []
          , divId "prevTrack" [], divId "nextTrack" []
          , sep
          -- , HH.div_ [sizes]
          , HH.div_ [strongText "Part:"]
          , divId "partLabel" []
          , divId "prevPart" [], divId "nextPart" []
          ]
        ]
  }
  where
    bpmLights = divId "bpm" []
    sep = strongText "|"
    strongText = HH.strong_ <<< pure <<< HH.text

setBpm :: RadioButton -> LoopCounter -> Dial -> Int -> Effect Unit
setBpm bpm counter loopDial n = do
  loopDial.setValue =<< getCounterRatio counter
  bpm.select n
  nextStep counter

updatePart :: RadioButton -> Select -> Control SelectItem -> Dial -> LoopCounter -> Dial -> Clip -> Effect Unit
updatePart bpm trackList sendOscTrack partLabel loopCounter loopDial (Clip clip) = do
  updateBpm bpm (round clip.measure)
  updatePartLabel partLabel (round clip.partIndex) (round clip.numOfParts)
  updateTrackList trackList sendOscTrack (round clip.trackIndex)
  updateLoopDial loopDial (round clip.nextAction)
  resetCounter loopCounter (round clip.beatSize * 2) -- why do we need to multiply???

updateBpm :: RadioButton -> Int -> Effect Unit
updateBpm bpm measure = do
  prevMeasure <- bpm.getNumberOfButtons
  when (prevMeasure /= measure) $ do
    bpm.setNumberOfButtons measure
    bpm.resize ((25.0 * 1.15 * toNumber measure) /\ 25.0)

-- | TODO use part maximum per track from config
updatePartLabel :: Dial -> Int -> Int -> Effect Unit
updatePartLabel partLabel partIndex numOfParts = do
  partLabel.setValue $
    if numOfParts > 0
      then toNumber (partIndex + 1) / toNumber numOfParts
      else 0.0
  if partIndex + 1 == numOfParts
    then partLabel.colorize AccentColor partLabelLastPartColor
    else partLabel.colorize AccentColor partLabelColor

-- | TODO do not send OSC on update
updateTrackList :: Select -> Control SelectItem -> Int -> Effect Unit
updateTrackList trackList sendOscTrack trackIndex = do
  index <- trackList.getSelectedIndex
  when (index /= trackIndex) $
    sendOscTrack.silent $ trackList.setSelectedIndex trackIndex

updateLoopDial :: Dial -> Int -> Effect Unit
updateLoopDial loopDial n = case n of
  -- loop action
  0 -> loopDial.colorize AccentColor loopColor
  -- play next action
  1 -> loopDial.colorize AccentColor playNextColor
  -- stop action
  2 -> loopDial.colorize AccentColor stopColor
  _ -> pure unit

toBpmName :: Int -> String
toBpmName index = "bpm-" <> show index

onButtonOn :: Effect Unit -> Boolean -> Effect Unit
onButtonOn act flag =
  when flag act

navButton :: String -> String -> Effect TextButton
navButton target name = newTextButtonBy target
  { size: Tuple 60.0 40.0
  , state: false
  , text: name
  , alternateText: Nothing
  }

navSelect :: String -> Array String -> Effect Select
navSelect target options = newSelectBy target
  { size: Tuple 150.0 50.0
  , options: options
  }

navDial :: String -> Effect Dial
navDial target = newDialBy target
  { size: 37.0 /\ 37.0
  , interaction: Radial
  , mode: AbsoluteDial
  , min: 0.0
  , max: 1.0
  , step: 0.0
  , value: 0.25
  }

type LoopCounter =
  { size :: Ref Int
  , step :: Ref Int
  }

initLoopCounter :: Int -> Effect LoopCounter
initLoopCounter size = do
  size <- Ref.new size
  step <- Ref.new 0
  pure {size, step}

nextStep :: LoopCounter -> Effect Unit
nextStep counter = do
  Ref.modify_ (\x -> x + 1) counter.step
  step <- Ref.read counter.step
  size <- Ref.read counter.size
  when (step >= size -1) $ Ref.write 0 counter.step

resetCounter :: LoopCounter -> Int -> Effect Unit
resetCounter counter size = do
  Ref.write size counter.size
  Ref.write 0 counter.step

getCounterRatio :: LoopCounter -> Effect Number
getCounterRatio counter = do
  size <- Ref.read counter.size
  step <- Ref.read counter.step
  pure $ if (size > 2)
    then toNumber (step + 1) / toNumber (size)
    else 0.0

defaultMeasure :: Int
defaultMeasure = 4
