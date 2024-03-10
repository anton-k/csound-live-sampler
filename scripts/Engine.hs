{-# Language
  DuplicateRecordFields
  OverloadedStrings
  OverloadedRecordDot
 #-}

-- | App to check that engine works.
--
-- It starts the engine and then feeds it with instructions to play various clips
import Csound.Core
import Live.Scene.Sampler.Engine

main :: IO ()
main =
  dacBy setTrace $ do
  -- writeCsdBy (setMa <> setDac) "tmp.csd" $ do
    instr1 <- newProc $ \(_size :: D) -> do
      writeOuts $ fromMono $ linseg [1, 0.5, 0] * osc 220

    instr2 <- newProc $ \(_size :: D) -> do
      writeOuts $ fromMono $ linseg [1, 0.5, 0] * osc 440

    engine <- newEngine

    instr3 <- newProc $ \(instrRef, bpm, size) ->
      engine.setPart $ Part
        { track = instrRef
        , clip = Clip
            { bpm = bpm
            , start = 0
            , changeRate = 16
            , beatSize = size
            , timeSize = 2
            }
        }

    engine.start
    play instr3
      [ Note 0 1 (instr1, 120, 4)
      , Note 5 1 (instr2, 60, 4)
      , Note 20 1 (instr1, 120, 1)
      , Note 60 1 (instr2, 30, 4)
      ]
