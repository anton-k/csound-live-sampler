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
  ) where

import Data.Text (Text)
import Control.Applicative (Alternative (..))
import Control.Monad
import Data.Aeson
import GHC.Generics (Generic)

data FxConfig =
  FxConfig
    { input :: FxInputType
    , chain :: FxChain
    }
  deriving (Generic, FromJSON, ToJSON)

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
  deriving (Generic, FromJSON, ToJSON)

data GroupFxConfig = GroupFxConfig
  { inputChannels :: [FxChannelInput]
  , outputChannel :: Int
  }
  deriving (Generic, FromJSON, ToJSON)

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

data FxChannelInput = FxChannelInput
  { channel :: Int
  , gain :: Float
  }
  deriving (Generic, FromJSON, ToJSON)

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

instance ToJSON FxUnit where
  toJSON = \case
    ReverbFx config -> object [ "reverb" .= config ]
    DelayFx config -> object [ "delay" .= config ]
    PingPongFx config -> object [ "pingPong" .= config ]
    MoogFx config -> object [ "moogFilter" .= config ]
    KorgFx config -> object [ "korgFilter" .= config ]
    BbcutFx config -> object [ "bbcut" .= config ]
    LimiterFx config -> object [ "limiter" .= config ]

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

data NamedFx a = NamedFx
  { name :: Text
  , fx :: a
  }
  deriving (Generic, FromJSON, ToJSON)

data ReverbConfig = ReverbConfig
  { size :: Float
  , damp :: Float
  , dryWet :: Float
  }
  deriving (Generic, FromJSON, ToJSON)

data DelayConfig = DelayConfig
  { repeatTime :: Float
  , damp :: Float
  , feedback :: Float
  , dryWet :: Float
  }
  deriving (Generic, FromJSON, ToJSON)

data PingPongConfig = PingPongConfig
  { repeatTime :: Float
  , damp :: Float
  , feedback :: Float
  , width :: Float
  , dryWet :: Float
  }
  deriving (Generic, FromJSON, ToJSON)

type MoogConfig = ResonantFilterConfig

type KorgConfig = ResonantFilterConfig

data ResonantFilterConfig = ResonantFilterConfig
  { cutoff :: Float
  , resonance :: Float
  , dryWet :: Float
  }
  deriving (Generic, FromJSON, ToJSON)

data BbcutConfig = BbcutConfig
  { subdiv :: Float
  , barlength :: Float
  , phrasebars :: Float
  , numrepeats :: Float
  , dryWet :: Float
  }
  deriving (Generic, FromJSON, ToJSON)

data LimiterConfig = LimiterConfig
  { maxVolume :: Float -- in range (0, 1), maximum volume, recommended 0.95
  }
  deriving (Generic, FromJSON, ToJSON)

