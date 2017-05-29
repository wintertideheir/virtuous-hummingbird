module Aikaterine where

import qualified Data.Text as T
import Data.Graph.Inductive
import qualified Data.Vector as V
import qualified Data.IntMap as IM

newtype RegionIdentifier = RegionIdentifier (V.Vector T.Text)

regionIdentifier :: T.Text -> Maybe RegionIdentifier
regionIdentifier pri =
  if any T.null pri'
  then Nothing
  else Just (RegionIdentifier (V.fromList pri'))
    where
      pri' = T.split (=='.') pri

data Position = Position { x :: Float
                         , y :: Float }

data Idea n = Idea { region     :: IM.Key
                   , name       :: T.Text
                   , value      :: n }

newtype Relation = Relation T.Text

data KnowledgeGraph n =
  KnowledgeGraph { regionM :: IM.IntMap RegionIdentifier
                 , relationM :: IM.IntMap Relation
                 , positionM :: IM.IntMap Position
                 , graph :: Gr (Idea n) IM.Key }
