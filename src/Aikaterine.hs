module Aikaterine where

import qualified Data.Text as T
import Data.Graph.Inductive
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
                   , value      :: n }

newtype Relation = Relation T.Text

newtype KnowledgeGraph n = KnowledgeGraph (Gr (Idea n) Relation)
