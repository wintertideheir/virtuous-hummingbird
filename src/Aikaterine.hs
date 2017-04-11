module Aikaterine where

import Data.Graph.Inductive
import Data.Map

newtype RegionIdentifier = RegionIdentifier [String]
  deriving (Eq, Ord)

regionIdentifier :: String -> Maybe RegionIdentifier
regionIdentifier "" = Just (RegionIdentifier [])
regionIdentifier ('.':_) = Nothing
regionIdentifier pri =
  regionIdentifier' pri [] [] False
    where
      regionIdentifier' :: String -> String -> [String] -> Bool -> Maybe RegionIdentifier
      regionIdentifier' ('.':_) _ _ True = Nothing
      regionIdentifier' ('.':x) q l False =
        regionIdentifier' x [] (q:l) True
      regionIdentifier' (x:xs) q l _ =
        regionIdentifier' xs (x:q) l False
      regionIdentifier' [] q l True = Nothing
      regionIdentifier' [] q l False = Just (RegionIdentifier (q:l))

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
          Nothing -> Just [i]
      getRegions [] rm = rm
      getRegions (i:is) rm =
        getRegions is (alter (updateRegion i) (region i) rm)
