module Test.Route
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Live.Scene.Mixer.Route
import Live.Scene.Mixer.Config
import Live.Scene.Mixer.Config qualified as Channel (ChannelConfig (..))
import Live.Scene.Fx.Config


tests :: TestTree
tests = testGroup "Routes"
  [ testCase "Simple channels" simpleChannels
  , testCase "Simple channels with FXs" simpleChannelsWithFx
  , testCase "Simple channels with sends" simpleChannelsWithSends
  , testCase "FX bus" fxBus
  , testCase "FX bus 2" fxBus2
  , testCase "Group" groupChannels
  , testCase "Group two layers" groupTwoLayers
  , testCase "Group two layers and FXs and sends" groupTwoLayersSends
  -- * sends
  -- * groups
  -- * groups + sends
  -- * FXs on groups + sends
  -- * 2-layer groups
  -- * 2-layer groups + FXs + sends
  ]

simpleChannels :: Assertion
simpleChannels =
  route config @?= routes
  where
    config =
      MixerConfig
        { master =
            MasterConfig
              { volume = 1
              , gain = Nothing
              , fxs = Nothing
              }
        , channels = replicate 4 channel
        }

    channel =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Nothing
        , fxs = Nothing
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
  route config @?= routes
  where
    config =
      MixerConfig
        { master =
            MasterConfig
              { volume = 1
              , gain = Nothing
              , fxs = Nothing
              }
        , channels = [channel, channelFx, channel]
        }

    channel =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Nothing
        , fxs = Nothing
        }

    channelFx =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Nothing
        , fxs = Just [reverb]
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
  route config @?= routes
  where
    config =
      MixerConfig
        { master =
            MasterConfig
              { volume = 1
              , gain = Nothing
              , fxs = Nothing
              }
        , channels = [channelSend 1, channel, channelSend 1]
        }

    channel =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Nothing
        , fxs = Nothing
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
  route config @?= routes
  where
    config =
      MixerConfig
        { master =
            MasterConfig
              { volume = 1
              , gain = Nothing
              , fxs = Nothing
              }
        , channels = [bus, channel, channel]
        }

    channel =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Just [SendConfig 0 0.5]
        , fxs = Nothing
        }

    bus =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Nothing
        , fxs = Just [reverb]
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
                {type_ = CopyOutput MasterOutput
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
  route config @?= routes
  where
    config =
      MixerConfig
        { master =
            MasterConfig
              { volume = 1
              , gain = Nothing
              , fxs = Nothing
              }
        , channels = [bus, channelSendTo 0, bus, channelSendTo 2]
        }

    channelSendTo n =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Just [SendConfig n 0.5]
        , fxs = Nothing
        }

    bus =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Nothing
        , fxs = Just [reverb]
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
  route config @?= routes
  where
    config =
      MixerConfig
        { master =
            MasterConfig
              { volume = 1
              , gain = Nothing
              , fxs = Nothing
              }
        , channels =
            [ channel { sends = Just [SendConfig 2 0.5] }
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
  route config @?= routes
  where
    config =
      MixerConfig
        { master =
            MasterConfig
              { volume = 1
              , gain = Nothing
              , fxs = Nothing
              }
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
        }

    channelOutputTo n = channel { output = Just n }

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
  route config @?= routes
  where
    config =
      MixerConfig
        { master =
            MasterConfig
              { volume = 1
              , gain = Nothing
              , fxs = Nothing
              }
        , channels =
            [ channel { sends = Just [SendConfig 6 0.5] }
            , channelOutputTo 0
            , (channelOutputTo 1) { Channel.fxs = Just [reverb] }
            , channelOutputTo 2
            , (channelOutputTo 2) { Channel.fxs = Just [reverb] }
            , channelOutputTo 1
            , channel { Channel.fxs = Just [reverb] }
            ]
        }

    channel =
      ChannelConfig
        { volume = 1
        , gain = Nothing
        , output = Nothing
        , sends = Nothing
        , fxs = Nothing
        }

    channelOutputTo n = channel { output = Just n }

    routes =
      Route
        [ GroupAct
            [ RouteAct
                { type_ = CopyOutput (ChannelOutput 1)
                , isActive = True
                , channel = 5}
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

reverb :: NamedFx FxUnit
reverb =
  NamedFx
    { name = "reverb"
    , fx = ReverbFx (ReverbConfig 1 1 1)
    }