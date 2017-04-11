module Aikaterine where

import Data.Graph.Inductive
import Data.Map

type RegionIdentifier = [String]

data Idea n = Idea { region     :: RegionIdentifier
                   , name       :: String
                   , importance :: Int
                   , value      :: n }

newtype Relation = Relation String

newtype KnowledgeNetwork n = KnowledgeNetwork (Gr (Idea n) Relation)

data Region n = Region RegionIdentifier [Idea n] [Region n]

regionsFromNetwork :: KnowledgeNetwork n -> Map RegionIdentifier [Idea n]
regionsFromNetwork (KnowledgeNetwork kn) =
  getRegions (Prelude.map snd (labNodes kn)) Data.Map.empty
    where
      updateRegion i mis =
        case mis of
          Just is -> Just (i:is)
          Nothing -> Nothing
      getRegions [] rm = rm
      getRegions (i:is) rm =
        getRegions is (alter (updateRegion i) (region i) rm)
