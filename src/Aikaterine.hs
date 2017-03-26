module Aikaterine where

import Data.Graph.Inductive

data Idea n = Idea { region     :: String
                   , name       :: String
                   , importance :: Int
                   , value      :: n }

newtype Relation = Relation String

newtype KnowledgeNetwork n = KnowledgeNetwork (Gr (Idea n) Relation)
