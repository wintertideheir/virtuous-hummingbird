module Aikaterine.Drawing where

import Data.Graph.Inductive
import qualified Data.Vector as V
import qualified Data.Map as M

data Position = Position { x :: Float
                         , y :: Float }

type NodePositions = M.Map Node Position

positionNodes :: Float -> V.Vector Node -> NodePositions
positionNodes minNodeDist ns =
  if minNodeDist < 1
  then undefined
  else positionNodes' 0 1 ns M.empty
    where
      positionNodes' l n ns m =
        if V.null ns
        then m
        else positionNodes' l' (ceiling (pi / (asin (minNodeDist / (2 * nodeDist')))))
             (V.drop n ns) (positionCircle ns m 0 nodeDist)
          where
            l' = l + 1
            nMin = min n (V.length ns)
            nodeDist = (fromIntegral l) * minNodeDist
            nodeDist' = (fromIntegral l') * minNodeDist
            theta = 2 * (pi :: Float) / (fromIntegral nMin)
            c = cos theta
            s = sin theta
            positionCircle ns m x y =
              if V.null ns
              then m
              else positionCircle (V.tail ns) (M.insert (V.head ns) (Position x y) m)
                                  (c*x - s*y) (s*x + c*y)
