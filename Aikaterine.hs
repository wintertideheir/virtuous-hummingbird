module Aikaterine where

import Data.Graph.Inductive

newtype Idea n = Idea (String, n)
newtype Relation = Relation String
newtype KnowledgeNetwork n = KnowledgeNetwork (Gr (Idea n) Relation)
