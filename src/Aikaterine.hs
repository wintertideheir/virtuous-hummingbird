module Aikaterine where

import qualified Data.Text as T
import Data.Maybe
import Data.Graph.Inductive
import Data.Sequence
import Data.Bits
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

data Region = Region RegionIdentifier (V.Vector Node) (V.Vector Region)

regionsFromNetwork :: KnowledgeGraph n -> Region
regionsFromNetwork (KnowledgeGraph kn) =
  createRegion (M.map V.fromList (getRegions (labNodes kn) M.empty)) 1
    where
      unwrapRegionIdentifier (RegionIdentifier ri) = ri
      createRegion m l =
        Region ri (fromMaybe V.empty (M.lookup ri m))
               (partitionRegions (M.delete ri m) V.empty)
          where
            ri = RegionIdentifier (V.take l (unwrapRegionIdentifier (fst (M.elemAt 0 m))))
            partitionRegions m v =
              if M.null m then v else partitionRegions (snd p) (V.cons (createRegion (fst p) (l+1)) v)
                where
                  sr = V.take (l+1) (unwrapRegionIdentifier (fst (M.elemAt 0 m)))
                  p = M.partitionWithKey (\ (RegionIdentifier k) _ -> (V.take (l+1) k) == sr) m
      updateRegion i mis =
        case mis of
          Just is -> Just (i:is)
          Nothing -> Just [i]
      getRegions [] rm = rm
      getRegions (i:is) rm =
        getRegions is (M.alter (updateRegion (fst i)) (region (snd i)) rm)

sortRegion :: Region -> KnowledgeGraph n -> Region
sortRegion (Region i n r) (KnowledgeGraph kn) =
  Region i (sortNodes n) (sortRegions r)
    where
      radixSortBy v f = radixSortBy' v f 0 (finiteBitSize (0 :: Int))
      radixSortBy' v f i m =
        if i > m then v else radixSortBy' ((fst part) V.++ (snd part)) f (i + 1) m
          where part = V.partition (\ x -> testBit (f x) i) v
      sortNodes n = radixSortBy n (deg kn)
      sortRegions r = V.map (\r -> sortRegion r (KnowledgeGraph kn)) (radixSortBy r degree)
      degree (Region _ n r) =
        (V.foldr (\ r -> (+) (degree r)) 0 r) + (V.foldr (\ n -> (+) (deg kn n)) 0 n)

data Position = Position { x :: Int
                         , y :: Int }

newtype NodePositions = NodePositions (M.Map Node Position)
