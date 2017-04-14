module Aikaterine where

import qualified Data.Text as T
import Data.Graph.Inductive
import Data.Sequence
import qualified Data.Vector as V
import qualified Data.Map as M

newtype RegionIdentifier = RegionIdentifier (V.Vector T.Text)
  deriving (Eq, Ord)

regionIdentifier :: T.Text -> Maybe RegionIdentifier
regionIdentifier pri =
  if any T.null pri'
  then Nothing
  else Just (RegionIdentifier (V.fromList pri'))
    where
      pri' = T.split (=='.') pri

data Idea n = Idea { region     :: RegionIdentifier
                   , name       :: T.Text
                   , importance :: Int
                   , value      :: n }

newtype Relation = Relation T.Text

newtype KnowledgeNetwork n = KnowledgeNetwork (Gr (Idea n) Relation)

data Region n = Region RegionIdentifier (V.Vector (Idea n)) (V.Vector (Region n))

regionsFromNetwork :: KnowledgeNetwork n -> M.Map RegionIdentifier (V.Vector (Idea n))
regionsFromNetwork (KnowledgeNetwork kn) =
  M.map V.fromList (getRegions (map snd (labNodes kn)) M.empty)
    where
      updateRegion i mis =
        case mis of
          Just is -> Just (i:is)
          Nothing -> Just [i]
      getRegions [] rm = rm
      getRegions (i:is) rm =
        getRegions is (M.alter (updateRegion i) (region i) rm)
