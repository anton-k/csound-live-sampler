module Scene.Mixer.AuxChannels
  ( withAuxChannels
  , auxChannelsSetup
  ) where

import Prelude
import Data.Maybe
import Scene.Mixer.Config
import Data.Array as Array
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Scene.Html
import Scene.Mixer.Config (toChannelName)
import Effect (Effect)
import Nexus.Ui.Core.Common as Ui
import Nexus.Ui.Core.Dial as Ui
import Data.Traversable (traverse)
import Action
import Osc.Client (newOscControl)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple (..))
import Common.Array (chunks, fillRow)

defColor0 = "#24bcbc"

withAuxChannels ::
  forall w i.
  Array MixerUiItem -> HH.HTML w i -> HH.HTML w i
withAuxChannels auxChannels body
  | Array.null auxChannels = body
  | otherwise =
      HH.div []
        [ body
        , HH.div [] [HH.p [] [HH.text "  "]]
        , auxChannelsHtml auxChannels
        ]

auxChannelsHtml ::
  forall w i. Array MixerUiItem -> HH.HTML w i
auxChannelsHtml channels =
  accordion "Aux" $
    HH.div [] (map toChannelRowHtml $ chunks maxChannelsPerRow channels)

toChannelRowHtml ::
  forall w i. Array MixerUiItem -> HH.HTML w i
toChannelRowHtml row =
  filledGrid maxChannelsPerRow $ map toChannelHtml row

toChannelHtml :: forall w i. MixerUiItem -> HH.HTML w i
toChannelHtml item =
  HH.div []
    [ divId (toChannelMeterId name) []
    , divId (toChannelVolumeId name) []
    , HH.div [] [HH.text name]
    ]
  where
    name = toChannelName item

maxChannelsPerRow :: Int
maxChannelsPerRow = 8

toChannelMeterId :: String -> String
toChannelMeterId name = name <> "AuxChannelMeter"

toChannelVolumeId :: String -> String
toChannelVolumeId name = name <> "AuxChannelVolume"

auxChannelsSetup :: Mixer -> Array MixerUiItem -> Effect (Map Int SetChannel)
auxChannelsSetup act channels = do
  setups <- traverse (\item -> map (Tuple item.channel) $ channelSetup act item) channels
  pure (Map.fromFoldable setups)

channelSetup :: Mixer -> MixerUiItem -> Effect SetChannel
channelSetup act item = do
  meterDial <- Ui.newDial ("#" <> toChannelMeterId (toChannelName item))
  meterDial.setValue 0.0

  volumeDial <- Ui.newDial ("#" <> toChannelVolumeId (toChannelName item))
  volumeDial.setValue item.volume
  sendOscVolume <- newOscControl $ \val ->
    act.setChannelVolume item.channel val
  volumeDial.on Ui.Change sendOscVolume.set

  pure $
    { setVolumeEnvelope: \volume -> meterDial.setValue volume
    , setVolume: \volume -> do
        sendOscVolume.silent $ volumeDial.setValue volume
    , setMute: \isMute ->
        if isMute
          then do
            meterDial.colorize Ui.AccentColor "#AAAAAA"
            volumeDial.colorize Ui.AccentColor "#AAAAAA"
          else do
            meterDial.colorize Ui.AccentColor defColor0
            volumeDial.colorize Ui.AccentColor defColor0
    }
