module Live.Scene.Midi.Mixer
  ( setupMidiFaders
  , muteInstr
  ) where

import Live.Scene.Mixer
import Live.Scene.Midi.Config
import Csound.Core
import Data.Boolean

setupMidiFaders :: ControllerConfig -> MixerConfig -> Mixer -> SE ()
setupMidiFaders controllers mixerConfig mixer = do
  setupMaster mixer fadersMidi mixerConfig.master
  setupChannels mixer fadersMidi mixerConfig.channels
  where
    fadersMidi = controllers.faders

setupChannels :: Mixer -> FadersMidiConfig -> [ChannelConfig] -> SE ()
setupChannels mixer fadersConfig configs = do
  mapM_
    (\(chn, config, channel) -> setupChannel mixer chn config channel)
    (zip3 midiChannels configs channels)
  where
    midiChannels = int <$> fadersConfig.channels
    channels = mixer.audio.inputs

setupChannel :: Mixer -> D -> ChannelConfig -> ChannelId -> SE ()
setupChannel mixer chn config channel = do
  initFader chn config.volume (mixer.modifyChannelVolume channel . const)

setupMaster :: Mixer -> FadersMidiConfig -> MasterConfig -> SE ()
setupMaster mixer fadersConfig config =
  initFader (int fadersConfig.master) config.volume (mixer.modifyMasterVolume . const)

initFader :: D -> Float -> (Sig -> SE ()) -> SE ()
initFader chn initVal write = do
  initc7  (CtrlInit 1 chn (float initVal))
  write kVol
  where
    kVol = gainslider $ kr $ ctrl7 1 chn 0 127

muteInstr :: Mixer -> SE BoolD -> [(D,ChannelId)] -> D -> SE ()
muteInstr mixer readShift muteNotes n = do
  isShift <- readShift
  whens
    (fmap (uncurry $ toMuteCase isShift n) muteNotes)
    (pure ())
  where
    toMuteCase :: BoolD -> D -> D -> ChannelId -> (BoolD, SE ())
    toMuteCase isShift notePressed noteId channelId =
      (notB isShift &&* notePressed ==* noteId, go)
      where
        go = mixer.toggleChannelMute channelId
