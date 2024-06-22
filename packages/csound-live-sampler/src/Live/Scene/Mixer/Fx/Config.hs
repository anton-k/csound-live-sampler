module Live.Scene.Mixer.Fx.Config (
  FxChain,
  FxUnit (..),
  ToolConfig (..),
  ReverbConfig (..),
  DelayConfig (..),
  PingPongConfig (..),
  MoogConfig,
  KorgConfig,
  ResonantFilterConfig (..),
  BbcutConfig (..),
  LimiterConfig (..),
  EqConfig (..),
  EqPoint (..),
  EqMode (..),
  MixerEqConfig (..),
) where

import Control.Applicative (Alternative (..))
import Data.Aeson
import Data.Aeson.TH qualified as Json
import Data.Text (Text)

-- | Chain of effect processors
type FxChain = [FxUnit]

data FxUnit
  = ToolFx ToolConfig
  | ReverbFx ReverbConfig
  | DelayFx DelayConfig
  | PingPongFx PingPongConfig
  | MoogFx MoogConfig
  | KorgFx KorgConfig
  | BbcutFx BbcutConfig
  | LimiterFx LimiterConfig
  | EqFx EqConfig
  | MixerEqFx MixerEqConfig

data ToolConfig = ToolConfig
  { name :: Text
  , volume :: Maybe Float
  , gain :: Maybe Float
  , pan :: Maybe Float
  , width :: Maybe Float
  }

data ReverbConfig = ReverbConfig
  { name :: Text
  , size :: Float
  , damp :: Float
  , dryWet :: Float
  }

data DelayConfig = DelayConfig
  { name :: Text
  , repeatTime :: Float
  , damp :: Float
  , feedback :: Float
  , dryWet :: Float
  }

data PingPongConfig = PingPongConfig
  { name :: Text
  , repeatTime :: Float
  , damp :: Float
  , feedback :: Float
  , width :: Float
  , dryWet :: Float
  }

type MoogConfig = ResonantFilterConfig

type KorgConfig = ResonantFilterConfig

data ResonantFilterConfig = ResonantFilterConfig
  { name :: Text
  , cutoff :: Float
  , resonance :: Float
  , dryWet :: Maybe Float
  }

data BbcutConfig = BbcutConfig
  { name :: Text
  , subdiv :: Float
  , barlength :: Float
  , phrasebars :: Float
  , numrepeats :: Float
  , dryWet :: Float
  }

data LimiterConfig = LimiterConfig
  { name :: Text
  , maxVolume :: Float -- in range (0, 1), maximum volume, recommended 0.95
  }

data EqConfig = EqConfig
  { name :: Text
  , points :: [EqPoint]
  , maxGainDb :: Maybe Float -- if nothing then 12 dB
  }

data EqPoint = EqPoint
  { mode :: EqMode
  , frequency :: Float
  , gain :: Float
  , width :: Maybe Float
  }

data EqMode
  = BandPassEq
  | LowShelfEq
  | HighShelfEq

data MixerEqConfig = MixerEqConfig
  { name :: Text
  , gains :: [Float]
  , frequencies :: [Float]
  , maxGainDb :: Maybe Float
  }

-- JSON instances

instance ToJSON EqMode where
  toJSON = \case
    LowShelfEq -> "lowShelf"
    HighShelfEq -> "highShelf"
    BandPassEq -> "bandPass"

instance FromJSON EqMode where
  parseJSON = withText "EqMode" $ \case
    "lowShelf" -> pure LowShelfEq
    "highShelf" -> pure HighShelfEq
    "bandPass" -> pure BandPassEq
    _ -> fail "Failed to parse"

$(Json.deriveJSON Json.defaultOptions ''ToolConfig)
$(Json.deriveJSON Json.defaultOptions ''ReverbConfig)
$(Json.deriveJSON Json.defaultOptions ''DelayConfig)
$(Json.deriveJSON Json.defaultOptions ''PingPongConfig)
$(Json.deriveJSON Json.defaultOptions ''LimiterConfig)
$(Json.deriveJSON Json.defaultOptions ''ResonantFilterConfig)
$(Json.deriveJSON Json.defaultOptions ''BbcutConfig)
$(Json.deriveJSON Json.defaultOptions ''EqPoint)
$(Json.deriveJSON Json.defaultOptions ''EqConfig)
$(Json.deriveJSON Json.defaultOptions ''MixerEqConfig)

instance ToJSON FxUnit where
  toJSON = \case
    ToolFx config -> object ["tool" .= config]
    ReverbFx config -> object ["reverb" .= config]
    DelayFx config -> object ["delay" .= config]
    PingPongFx config -> object ["pingPong" .= config]
    MoogFx config -> object ["moogFilter" .= config]
    KorgFx config -> object ["korgFilter" .= config]
    BbcutFx config -> object ["bbcut" .= config]
    LimiterFx config -> object ["limiter" .= config]
    EqFx config -> object ["eq" .= config]
    MixerEqFx config -> object ["mixerEq" .= config]

instance FromJSON FxUnit where
  parseJSON = withObject "FxUnit" $ \obj ->
    let
      parseBy cons field = fmap cons (obj .: field)
     in
      parseBy ToolFx "tool"
        <|> parseBy ReverbFx "reverb"
        <|> parseBy DelayFx "delay"
        <|> parseBy PingPongFx "pingPong"
        <|> parseBy MoogFx "moogFilter"
        <|> parseBy KorgFx "korgFilter"
        <|> parseBy BbcutFx "bbcut"
        <|> parseBy LimiterFx "limiter"
        <|> parseBy EqFx "eq"
        <|> parseBy MixerEqFx "mixerEq"
