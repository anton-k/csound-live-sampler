module Live.Scene.Osc.Output (
  setupOscOutput,
) where

import Csound.Core
import Data.Text qualified as Text
import Live.Scene.Osc.Config
import Live.Scene.Sampler
import Live.Scene.Types

setupOscOutput :: Scene -> OscOutputConfig -> SE ()
setupOscOutput scene config = do
  instr <- newProc $ \() -> do
    sendTicks scene.sampler config
  play instr [Note 0 (-1) ()]

send :: (Tuple a) => OscOutputConfig -> Sig -> Str -> a -> SE ()
send config kwhen dest values =
  oscSend kwhen (fromString $ Text.unpack config.address) (int config.port) dest values

sendTicks :: Sampler -> OscOutputConfig -> SE ()
sendTicks sampler config = do
  currentBeat <- sampler.currentBeat
  ticks <- sampler.readTicks
  send config ticks "/bpm/beats" currentBeat

--  when1 (ticks `equals` 1) $
--    printks "Beat: %d\n" 0 currentBeat
