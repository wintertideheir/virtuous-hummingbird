-- |Every basic type. Two particularly important ones are 'KnowledgeGraph' and
-- 'GraphView'.
module Aikaterine where

import qualified Data.Text as T
import Data.Graph.Inductive
import qualified Data.Vector as V
import qualified Data.IntMap as IM
import qualified Data.Word as W

-- |A sequence of identifiers denoting a category of ideas, or a region
-- spatially/aesthetically.
newtype RegionIdentifier = RegionIdentifier (V.Vector T.Text)

-- |The smart constructor for the 'RegionIdentifier' type. Region identifiers
-- are period delimited sequences of text.
regionIdentifier :: T.Text -> Maybe RegionIdentifier
regionIdentifier pri =
  if any T.null pri'
  then Nothing
  else Just (RegionIdentifier (V.fromList pri'))
    where
      pri' = T.split (=='.') pri

-- |A coordinate, point or position in the Cartesian coordinate system.
data Position = Position { x :: Float
                         , y :: Float }

-- |A mapping of 'Node's (i.e. 'Idea's) to 'Position's. 'Node's are type
-- synonyms for 'Int's, therefore 'Data.Map.Map' 'Node' 'Position' is equivalent
-- to 'IM.IntMap' (which is more performant).
type NodePositions = IM.IntMap Position

-- |A square.
data Square = Square { center    :: Position
                     -- ^The center of the square.
                     , side      :: Float
                     -- ^The length of a side.
                     }

-- |A quad tree of 'NodePositions'. The 'Branch' constructor has four 'KdTree'
-- parameters, which correspond to the up-left, up-right, down-left and
-- down-right quadrants respectively.
data QuadTree = Branch QuadTree QuadTree QuadTree QuadTree
              | Leaf NodePositions

-- |An thought, argument or assertion.
data Idea n = Idea { region :: IM.Key -- ^The map key of a category or region.
                   , name   :: T.Text -- ^An optional name.
                   , value  :: n      -- ^The contents of the idea.
                   }

-- |A word or phrase describing the relation between two 'Idea's.
newtype Relation = Relation T.Text

-- |A graph of 'Idea's and 'Relation's between them, mapping keys to
-- 'RegionIdentifier's and 'Relation's.
data KnowledgeGraph n = KnowledgeGraph { regionM   :: IM.IntMap RegionIdentifier
                                       -- ^A mapping of 'Int' indices to
                                       -- 'RegionIdentifier's (for performance).
                                       , relationM :: IM.IntMap Relation
                                       -- ^A mapping of 'Int' indices to
                                       -- 'Relation's (for performance).
                                       , quadTree  :: (Square, QuadTree)
                                       -- ^A spatial database of Idea's.
                                       , graph     :: Gr (Idea n) IM.Key
                                       -- ^A graph of 'Idea's and the relations
                                       -- between them.
                                       }
