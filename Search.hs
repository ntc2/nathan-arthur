module Search where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

import Graph
import Parse

-- these would be completely symmetric without the phantoms ...
type EdgeWeighting = Edge -> Double
distanceWeight, reversalWeight :: EdgeWeighting
distanceWeight (((s,_),d),((s',_),d')) =
  if d /= d' ||
     -- don't count phantom transitions
     s < 0 || s' < 0
  then 0 else 1 -- could (conf)use 'fromEnum' here ...
reversalWeight ((_,    d),(_,     d')) =
  if d == d'
  then 0 else 1

-- updating the queue when shortest path distances change does not
-- sound fun, so: the Q may contain multiple copies of each Node.
-- separately track which nodes are visited, and then skip already
-- visited nodes when removing them from the Q.  this makes the Q as
-- large as E, as opposed to V when no duplicates, but we have E =
-- \Theta(V), so no asymptotic penalty.

type PrioQ         = S.Set (Double, Node)
type NodeWeighting = M.Map Node Double
type StepCount = Integer
type ParentPtrs = M.Map Node Node
-- cost of dikjstra is # edges = sum (map length (M.toList adj))
dijkstra :: EdgeWeighting -> Node -> Adj -> (NodeWeighting, ParentPtrs)
dijkstra weight n adj = search' d v q pp where
  search' :: NodeWeighting -> S.Set Node -> PrioQ -> ParentPtrs
          -> (NodeWeighting, ParentPtrs)
  search' d visited q pp = case S.minView q of
    Nothing          -> (d,pp)
    Just ((w,n), q') -> if n `S.member` visited
                        -- skip already visited
                        then search' d  visited  q'  pp
                        else search' d' visited' q'' pp' where
      -- update distances
      (d',q'',pp') = foldr update (d,q',pp) (adj M.! n) where
        update :: Node -> (NodeWeighting, PrioQ, ParentPtrs)
               -> (NodeWeighting, PrioQ, ParentPtrs)
        update m (d,q,pp) = if d M.! m > w'
                            then (M.insert m w'   d
                                 ,S.insert (w',m) q
                                 ,M.insert m n    pp)
                            else (d,q,pp)
          where w' = w + weight (n,m)
      -- mark visited
      visited' = S.insert n visited
  -- initial values:
  d = M.insert n 0           -- distance is 0 to self and infinity o/w
    $ M.fromList [(m, 1/0) | m <- M.keys adj]
  v = S.empty                -- nothing visited
  q = S.insert (0,n) S.empty -- self on queue and dist 0
  pp = M.empty               -- no parent pointers

extractPath :: ParentPtrs -> Node -> [Node]
extractPath pp n = reverse $ f pp n where
  f pp n = case  n `M.lookup` pp of
             Nothing -> []
             Just n' -> n' : f pp n'

-- Search.test distanceWeight "./transform-bug.trunk-branch-self-loop.txt"
testWeighting w f = print . map w . makeGraph . phantomize =<< file f

-- find distances from first node in adj
-- testDijkstra distanceWeight "./sample1.txt"
testDijkstra w f = print . (\adj -> dijkstra w (M.keys adj !! 0) adj) . makeAdj . makeGraph . phantomize =<< file f
