-- | Audio generator. It has inputs and outputs
module Live.Scene.Gen
  ( ChannelId (..)
  , Gen (..)
  , emptyGen
  , par
  , (|>)
  , runGen
  ) where

import Csound.Core hiding (gen)
import Control.Monad

newtype ChannelId = ChannelId Int
  deriving newtype (Show, Eq, Ord)

data Gen = Gen
  { read :: ChannelId -> SE Sig2
  , write :: ChannelId -> Sig2 -> SE ()
  , inputs :: [ChannelId]
  , outputs :: [ChannelId]
  , update :: SE ()
  }

runGen :: Gen -> [Sig2] -> SE [Sig2]
runGen gen audioIns = do
  zipWithM_ gen.write gen.inputs audioIns
  gen.update
  mapM gen.read gen.outputs

emptyGen :: Gen
emptyGen = Gen
  { read = const $ pure 0
  , write = const $ const $ pure ()
  , inputs = []
  , outputs = []
  , update = pure ()
  }

par :: Gen -> Gen -> Gen
par a b =
  Gen
    { read = \(ChannelId n) ->
        if n < aSizeOuts
          then a.read (ChannelId n)
          else b.read (ChannelId (n - aSizeOuts))

    , write = \(ChannelId n) audio ->
        if (n < aSizeIns)
          then a.write (ChannelId n) audio
          else a.write (ChannelId (n - aSizeIns)) audio

    , inputs = a.inputs <> b.inputs
    , outputs = a.outputs <> b.outputs
    , update = do
        a.update
        b.update
    }
  where
    aSizeIns = length a.inputs
    aSizeOuts = length a.outputs

-- runGen (sampler.gen |> mixer.gen) []
(|>) :: Gen -> Gen -> Gen
(|>) a b = do
  Gen
    { read = b.read
    , write = a.write
    , inputs = a.inputs
    , outputs = b.outputs
    , update = do
        a.update
        b.update
        zipWithM_ (\aOut bIn -> b.write bIn =<< a.read aOut) a.outputs b.inputs
    }
