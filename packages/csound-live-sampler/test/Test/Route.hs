module Test.Route (
  tests,
) where

import Live.Scene.Common (ChannelId (..))
import Live.Scene.Mixer.Config
import Live.Scene.Mixer.Config qualified as Channel (ChannelConfig (..))
import Live.Scene.Mixer.Fx.Config
import Live.Scene.Mixer.Route.DependencyGraph
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Routes"
    [ testCase "Simple channels" simpleChannels
    , testCase "Simple channels with FXs" simpleChannelsWithFx
    , testCase "Simple channels with sends" simpleChannelsWithSends
    , testCase "FX bus" fxBus
    , testCase "FX bus 2" fxBus2
    , testCase "Group" groupChannels
    , testCase "Group two layers" groupTwoLayers
    , testCase "Group two layers and FXs and sends" groupTwoLayersSends
    ]

-- FX test example
reverb :: FxUnit
reverb = ReverbFx (ReverbConfig "reverb" 1 1 1)

simpleChannels :: Assertion
simpleChannels =
  orderDependencies config @?= routes
  where
    config =
      MixerConfig
        { master = Nothing
        , channels = replicate 4 channel
        }

    channel =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Nothing
        , fxs = Nothing
        , name = Nothing
        }

    routes =
      Route
        [ GroupAct
            [ RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 3
                }
            , RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 2
                }
            , RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 1
                }
            , RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 0
                }
            ]
        ]

simpleChannelsWithFx :: Assertion
simpleChannelsWithFx =
  orderDependencies config @?= routes
  where
    config =
      MixerConfig
        { master = Nothing
        , channels = [channel, channelFx, channel]
        }

    channel =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Nothing
        , fxs = Nothing
        , name = Nothing
        }

    channelFx =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Nothing
        , fxs = Just [reverb]
        , name = Nothing
        }

    routes =
      Route
        [ GroupAct
            [ RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 2
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = ApplyFx
                , isActive = True
                , channel = 1
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 1
                }
            , RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 0
                }
            ]
        ]

simpleChannelsWithSends :: Assertion
simpleChannelsWithSends =
  orderDependencies config @?= routes
  where
    config =
      MixerConfig
        { master = Nothing
        , channels = [channelSend 1, channel, channelSend 1]
        }

    channel =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Nothing
        , fxs = Nothing
        , name = Nothing
        }

    channelSend n =
      channel
        { sends = Just [SendConfig n 1]
        }

    routes =
      Route
        [ GroupAct
            [ RouteAct
                { type_ = CopySends
                , isActive = True
                , channel = 2
                }
            , RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 2
                }
            , RouteAct
                { type_ = CopySends
                , isActive = True
                , channel = 0
                }
            , RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 1
                }
            , RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 0
                }
            ]
        ]

fxBus :: Assertion
fxBus =
  orderDependencies config @?= routes
  where
    config =
      MixerConfig
        { master = Nothing
        , channels = [bus, channel, channel]
        }

    channel =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Just [SendConfig 0 0.5]
        , fxs = Nothing
        , name = Nothing
        }

    bus =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Nothing
        , fxs = Just [reverb]
        , name = Nothing
        }

    routes =
      Route
        [ GroupAct
            [ RouteAct
                { type_ = CopySends
                , isActive = True
                , channel = 2
                }
            , RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 2
                }
            , RouteAct
                { type_ = CopySends
                , isActive = True
                , channel = 1
                }
            , RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 1
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = ApplyFx
                , isActive = True
                , channel = 0
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 0
                }
            ]
        ]

fxBus2 :: Assertion
fxBus2 =
  orderDependencies config @?= routes
  where
    config =
      MixerConfig
        { master = Nothing
        , channels = [bus, channelSendTo 0, bus, channelSendTo 2]
        }

    channelSendTo n =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Just [SendConfig n 0.5]
        , fxs = Nothing
        , name = Nothing
        }

    bus =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Nothing
        , fxs = Just [reverb]
        , name = Nothing
        }

    routes =
      Route
        [ GroupAct
            [ RouteAct
                { type_ = CopySends
                , isActive = True
                , channel = 3
                }
            , RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 3
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = ApplyFx
                , isActive = True
                , channel = 2
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 2
                }
            , RouteAct
                { type_ = CopySends
                , isActive = True
                , channel = 1
                }
            , RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 1
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = ApplyFx
                , isActive = True
                , channel = 0
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 0
                }
            ]
        ]

groupChannels :: Assertion
groupChannels =
  orderDependencies config @?= routes
  where
    config =
      MixerConfig
        { master = Nothing
        , channels =
            [ channel{sends = Just [SendConfig 2 0.5]}
            , channelOutputTo 0
            , bus
            , channelOutputTo 0
            ]
        }

    channel =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Nothing
        , fxs = Nothing
        , name = Nothing
        }

    channelOutputTo n =
      channel
        { output = Just n
        , fxs = Just [reverb]
        }

    bus =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Nothing
        , fxs = Just [reverb]
        , name = Nothing
        }

    routes =
      Route
        [ GroupAct
            [ RouteAct
                { type_ = ApplyFx
                , isActive = True
                , channel = 3
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = CopyOutput (ChannelOutput 0)
                , isActive = True
                , channel = 3
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = ApplyFx
                , isActive = True
                , channel = 1
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = CopyOutput (ChannelOutput 0)
                , isActive = True
                , channel = 1
                }
            , RouteAct
                { type_ = CopySends
                , isActive = True
                , channel = 0
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = ApplyFx
                , isActive = True
                , channel = 2
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 2
                }
            , RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 0
                }
            ]
        ]

groupTwoLayers :: Assertion
groupTwoLayers =
  orderDependencies config @?= routes
  where
    config =
      MixerConfig
        { master = Nothing
        , channels =
            [ channel
            , channelOutputTo 0
            , channelOutputTo 1
            , channelOutputTo 2
            , channelOutputTo 2
            , channelOutputTo 1
            ]
        }

    channel =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Nothing
        , fxs = Nothing
        , name = Nothing
        }

    channelOutputTo n = channel{output = Just n}

    routes =
      Route
        [ GroupAct
            [ RouteAct
                { type_ = CopyOutput (ChannelOutput 1)
                , isActive = True
                , channel = 5
                }
            , RouteAct
                { type_ = CopyOutput (ChannelOutput 2)
                , isActive = True
                , channel = 4
                }
            , RouteAct
                { type_ = CopyOutput (ChannelOutput 2)
                , isActive = True
                , channel = 3
                }
            , RouteAct
                { type_ = CopyOutput (ChannelOutput 1)
                , isActive = True
                , channel = 2
                }
            , RouteAct
                { type_ = CopyOutput (ChannelOutput 0)
                , isActive = True
                , channel = 1
                }
            , RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 0
                }
            ]
        ]

groupTwoLayersSends :: Assertion
groupTwoLayersSends =
  orderDependencies config @?= routes
  where
    config =
      MixerConfig
        { master = Nothing
        , channels =
            [ channel{sends = Just [SendConfig 6 0.5]}
            , channelOutputTo 0
            , (channelOutputTo 1){Channel.fxs = Just [reverb]}
            , channelOutputTo 2
            , (channelOutputTo 2){Channel.fxs = Just [reverb]}
            , channelOutputTo 1
            , channel{Channel.fxs = Just [reverb]}
            ]
        }

    channel =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Nothing
        , fxs = Nothing
        , name = Nothing
        }

    channelOutputTo n = channel{output = Just n}

    routes =
      Route
        [ GroupAct
            [ RouteAct
                { type_ = CopyOutput (ChannelOutput 1)
                , isActive = True
                , channel = 5
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = ApplyFx
                , isActive = True
                , channel = 4
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = CopyOutput (ChannelOutput 2)
                , isActive = True
                , channel = 4
                }
            , RouteAct
                { type_ = CopyOutput (ChannelOutput 2)
                , isActive = True
                , channel = 3
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = ApplyFx
                , isActive = True
                , channel = 2
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = CopyOutput (ChannelOutput 1)
                , isActive = True
                , channel = 2
                }
            , RouteAct
                { type_ = CopyOutput (ChannelOutput 0)
                , isActive = True
                , channel = 1
                }
            , RouteAct
                { type_ = CopySends
                , isActive = True
                , channel = 0
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = ApplyFx
                , isActive = True
                , channel = 6
                }
            ]
        , GroupAct
            [ RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 6
                }
            , RouteAct
                { type_ = CopyOutput MasterOutput
                , isActive = True
                , channel = 0
                }
            ]
        ]

deriving newtype instance Num ChannelId
