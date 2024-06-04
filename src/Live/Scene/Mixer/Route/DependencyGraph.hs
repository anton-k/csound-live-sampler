-- | Orders flow of signals for channels on mixer
--
-- We need to respect this flow for each channel:
--
-- * first apply FXs
-- * second copy sends to subsequent channels
-- * third copy outputs
module Live.Scene.Mixer.Route.DependencyGraph
  ( Route (..)
  , GroupAct (..)
  , RouteAct (..)
  , RouteActType (..)
  , ChannelOutput (..)
  , orderDependencies
  ) where

import Data.Bifunctor (second)
import Live.Scene.Mixer.Config
import Data.Graph (Graph, Vertex)
import Data.Graph qualified as Graph
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe
import Data.List qualified as List
import Data.Array qualified as Array

data RouteGraph = RouteGraph
  { graph :: Graph
  , content :: IntMap RouteAct
  }
  deriving (Show, Eq)

newtype Route = Route [GroupAct]
  deriving (Show, Eq)

newtype GroupAct = GroupAct { acts :: [RouteAct] }
  deriving (Show, Eq)

data RouteAct = RouteAct
  { type_ :: RouteActType
  , isActive :: Bool
  , channel :: Int
  }
  deriving (Show, Eq)

data RouteActType
  = CopyOutput ChannelOutput
  | ApplyFx
  | CopySends
  deriving (Show, Eq)

data ChannelOutput
  = ChannelOutput Int
  | MasterOutput
  deriving (Show, Eq)

orderDependencies :: MixerConfig -> Route
orderDependencies config = Route $ toGroupActs sortedActs
  where
    routeGraph = toRouteGraph config

    sortedVertices = Graph.topSort routeGraph.graph

    sortedActs = mapMaybe (flip IntMap.lookup routeGraph.content) sortedVertices

-- | We can group in one procedure all actions that does not contain application
-- of FXs as each FX should be executed in it's own instrument.
toGroupActs :: [RouteAct] -> [GroupAct]
toGroupActs acts =
  GroupAct <$> List.groupBy sameActs (filter (.isActive) acts)
  where
    sameActs :: RouteAct -> RouteAct -> Bool
    sameActs actA actB =
      not (isFxAct actA || isFxAct actB)

    isFxAct :: RouteAct -> Bool
    isFxAct act = act.type_ == ApplyFx

toRouteGraph :: MixerConfig -> RouteGraph
toRouteGraph config =
  RouteGraph
    { graph = Array.array (0, length config.channels * 3 - 1) $ fmap (second snd) $ List.sortOn fst vertices
    , content = fst <$> IntMap.fromList vertices
    }
  where
    vertices = concat $ zipWith channelToVertices [0..] config.channels

-- | For every channel we produce 3 vertices:
--
-- * 1: apply fxs
-- * 2: copy sends
-- * 3: copy output
--
-- those stages are implemented in line. We add additional successors
-- for send destination channels and output channels.
--
-- Output to master produce no successor.
channelToVertices :: Int -> ChannelConfig -> [(Vertex, (RouteAct, [Vertex]))]
channelToVertices channelIndex config =
  [ applyFx, copySends, copyOutput ]
  where
    applyFx =
      ( toApplyFxIndex channelIndex
      , ( RouteAct
            { type_ = ApplyFx
            , isActive = isJust config.fxs
            , channel = channelIndex
            }
        , applyFxSuccessors
        )
      )

    copySends =
      ( toCopySendsIndex channelIndex
      , ( RouteAct
            { type_ = CopySends
            , isActive = isJust config.sends
            , channel = channelIndex
            }
        , copySendsSuccessors
        )
      )

    copyOutput =
      ( toCopyOutpIndex channelIndex
      , ( RouteAct
            { type_ = CopyOutput $ maybe MasterOutput ChannelOutput config.output
            , isActive = True
            , channel = channelIndex
            }
        , copyOutputSuccessors
        )
      )

    applyFxSuccessors = [toCopySendsIndex channelIndex]

    copySendsSuccessors =
      toCopyOutpIndex channelIndex : fmap toApplyFxIndex sends
      where
        sends = fmap (.channel) $ fromMaybe [] config.sends

    copyOutputSuccessors = fmap toApplyFxIndex outputs
      where
        outputs = maybe [] pure config.output

    toApplyFxIndex :: Int -> Int
    toApplyFxIndex channel = 3 * channel

    toCopySendsIndex :: Int -> Int
    toCopySendsIndex channel = 3 * channel + 1

    toCopyOutpIndex :: Int -> Int
    toCopyOutpIndex channel = 3 * channel + 2


