module Live.Scene.Fx.Config
  ( FxConfig (..)
  , FxInputType (..)
  , FxChannelInput (..)
  , FxChain
  , FxUnit (..)
  , NamedFx (..)
  , ReverbConfig (..)
  , DelayConfig (..)
  , PingPongConfig (..)
  , MoogConfig
  , KorgConfig
  , ResonantFilterConfig (..)
  , BbcutConfig (..)
  , LimiterConfig (..)
  , GroupFxConfig (..)
  , ChannelFxConfig (..)
  , EqConfig (..)
  , EqPoint (..)
  , EqMode (..)
  , MixerEqConfig (..)
  ) where

import Data.Text (Text)
import Control.Applicative (Alternative (..))
import Control.Monad
import Data.Aeson
import Data.Aeson.TH qualified as Json

data FxConfig =
  FxConfig
    { input :: FxInputType
    , chain :: FxChain
    }

data FxInputType
  = MasterFx
    -- ^ FX is applied to master channel
  | ChannelFx ChannelFxConfig
    -- ^ FX is applied to a single channel
  | GroupFx GroupFxConfig
    -- ^ FX reads several channels with gain inputs and produces result
    -- on separate channel

data ChannelFxConfig =
  ChannelFxConfig
    { channel :: Int }

data GroupFxConfig = GroupFxConfig
  { inputChannels :: [FxChannelInput]
  , outputChannel :: Int
  }

data FxChannelInput = FxChannelInput
  { channel :: Int
  , gain :: Float
  }

-- | Chain of effect processors
type FxChain = [NamedFx FxUnit]

data FxUnit
  = ReverbFx ReverbConfig
  | DelayFx DelayConfig
  | PingPongFx PingPongConfig
  | MoogFx MoogConfig
  | KorgFx KorgConfig
  | BbcutFx BbcutConfig
  | LimiterFx LimiterConfig
  | EqFx EqConfig
  | MixerEqFx MixerEqConfig

data NamedFx a = NamedFx
  { name :: Text
  , fx :: a
  }

data ReverbConfig = ReverbConfig
  { size :: Float
  , damp :: Float
  , dryWet :: Float
  }

data DelayConfig = DelayConfig
  { repeatTime :: Float
  , damp :: Float
  , feedback :: Float
  , dryWet :: Float
  }

data PingPongConfig = PingPongConfig
  { repeatTime :: Float
  , damp :: Float
  , feedback :: Float
  , width :: Float
  , dryWet :: Float
  }

type MoogConfig = ResonantFilterConfig

type KorgConfig = ResonantFilterConfig

data ResonantFilterConfig = ResonantFilterConfig
  { cutoff :: Float
  , resonance :: Float
  , dryWet :: Float
  }

data BbcutConfig = BbcutConfig
  { subdiv :: Float
  , barlength :: Float
  , phrasebars :: Float
  , numrepeats :: Float
  , dryWet :: Float
  }

data LimiterConfig = LimiterConfig
  { maxVolume :: Float -- in range (0, 1), maximum volume, recommended 0.95
  }

newtype EqConfig = EqConfig
  { points :: [EqPoint]
  }

data EqPoint = EqPoint
  { mode :: EqMode
  , frequency :: Float
  , gain :: Float
  , width :: Maybe Float
  }

data EqMode
  = LowPassEq
  | HighPassEq
  | BandPassEq
  | NotchEq
  | LowShelfEq
  | HighShelfEq

data MixerEqConfig = MixerEqConfig
  { gains :: [Float]
  }

-- JSON instances

$(Json.deriveJSON Json.defaultOptions ''FxChannelInput)
$(Json.deriveJSON Json.defaultOptions ''NamedFx)
$(Json.deriveJSON Json.defaultOptions ''ReverbConfig)
$(Json.deriveJSON Json.defaultOptions ''DelayConfig)
$(Json.deriveJSON Json.defaultOptions ''PingPongConfig)
$(Json.deriveJSON Json.defaultOptions ''LimiterConfig)
$(Json.deriveJSON Json.defaultOptions ''ResonantFilterConfig)
$(Json.deriveJSON Json.defaultOptions ''BbcutConfig)
$(Json.deriveJSON Json.defaultOptions ''EqMode)
$(Json.deriveJSON Json.defaultOptions ''EqPoint)
$(Json.deriveJSON Json.defaultOptions ''EqConfig)
$(Json.deriveJSON Json.defaultOptions ''MixerEqConfig)

instance ToJSON FxUnit where
  toJSON = \case
    ReverbFx config -> object [ "reverb" .= config ]
    DelayFx config -> object [ "delay" .= config ]
    PingPongFx config -> object [ "pingPong" .= config ]
    MoogFx config -> object [ "moogFilter" .= config ]
    KorgFx config -> object [ "korgFilter" .= config ]
    BbcutFx config -> object [ "bbcut" .= config ]
    LimiterFx config -> object [ "limiter" .= config ]
    EqFx config -> object [ "eq" .= config ]
    MixerEqFx config -> object [ "mixerEq" .= config ]

instance FromJSON FxUnit where
  parseJSON = withObject "FxUnit" $ \obj ->
    let
      parseBy cons field = fmap cons (obj .: field)
    in
          parseBy ReverbFx "reverb"
      <|> parseBy DelayFx "delay"
      <|> parseBy PingPongFx "pingPong"
      <|> parseBy MoogFx "moogFilter"
      <|> parseBy KorgFx "korgFilter"
      <|> parseBy BbcutFx "bbcut"
      <|> parseBy LimiterFx "limiter"
      <|> parseBy EqFx "eq"
      <|> parseBy MixerEqFx "mixerEq"

$(Json.deriveJSON Json.defaultOptions ''ChannelFxConfig)
$(Json.deriveJSON Json.defaultOptions ''GroupFxConfig)

instance ToJSON FxInputType where
  toJSON = \case
    MasterFx -> "masterFx"
    ChannelFx config -> object [ "channelFx" .= config ]
    GroupFx config -> object [ "groupFx" .= config ]

instance FromJSON FxInputType where
  parseJSON v =
        withText "FxInputType" (\case
          "masterFx" -> pure MasterFx
          _ -> mzero
        ) v
    <|> withObject "FxInputType" (\obj ->
              fmap ChannelFx (obj .: "channelFx")
          <|> fmap GroupFx (obj .: "groupFx")
        ) v

$(Json.deriveJSON Json.defaultOptions ''FxConfig)
