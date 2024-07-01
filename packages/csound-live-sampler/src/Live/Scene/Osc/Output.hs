module Live.Scene.Osc.Output (
  setupOscOutput,
) where

import Csound.Core
import Data.Maybe
import Data.Text qualified as Text
import Live.Scene.Common (ChannelId (..))
import Live.Scene.Mixer
import Live.Scene.Osc.Config
import Live.Scene.Sampler
import Live.Scene.Sampler.Engine (toAbsTimeRate)
import Live.Scene.Types

setupOscOutput :: Scene -> OscOutputConfig ChannelId -> SE ()
setupOscOutput scene config = do
  instr <- newProc $ \() -> do
    sendTicks scene.sampler config
    -- isTick <- scene.sampler.readTicks
    bpm <- scene.sampler.readBpm
    let
      isTick = metro (toAbsTimeRate bpm 0.25)
    mapM_ (sendVolume config isTick bpm scene.mixer) (fromMaybe [] config.channels)
  play instr [Note 0 (-1) ()]

sendTicks :: Sampler -> OscOutputConfig ChannelId -> SE ()
sendTicks sampler config = do
  currentBeat <- sampler.currentBeat
  ticks <- sampler.readTicks
  send config ticks "/bpm/beats" currentBeat

sendVolume :: OscOutputConfig ChannelId -> Sig -> Sig -> Mixer -> ChannelId -> SE ()
sendVolume config isTick bpm mixer channelId = do
  asig <- mixer.readChannel channelId

  let
    dt = recip $ toAbsTimeRate bpm 2
    env = 5 * follow2 (toMono asig) 0.1 0.5 -- dt dt
  send config isTick addr env
  where
    addr = (fromString $ "/channel/" <> toChannelAddr channelId <> "/volume/envelope")
    toChannelAddr (ChannelId n) = show (n + 1)

--  when1 (ticks `equals` 1) $
--    printks "Beat: %d\n" 0 currentBeat

send :: (Tuple a) => OscOutputConfig ChannelId -> Sig -> Str -> a -> SE ()
send config kwhen dest values =
  oscSend kwhen (fromString $ Text.unpack config.address) (int config.port) dest values
