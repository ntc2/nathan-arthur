module Search where

-- cabal install meldable-heap
import qualified Data.MeldableHeap as H

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

type PrioQ = H.PQ (Double, Node)
type NodeWeighting = M.Map Node Double
-- TODO: parent pointers, execution cost
dijkstra :: EdgeWeighting -> Adj -> Node -> NodeWeighting
dijkstra weight adj n = search d v q where
  search :: NodeWeighting -> S.Set Node -> PrioQ -> NodeWeighting
  search d visited q = case H.extractMin q of
    Nothing -> d         -- skip already visited
    Just ((w,n'), q') -> if n' `S.member` visited
                         then search d  visited  q'
                         else search d' visited' q'' where
      -- update distances
      (d',q'') = foldr update (d,q') (adj M.! n') where
        update :: Node -> (NodeWeighting, PrioQ) -> (NodeWeighting, PrioQ)
        update m (d,q) = if d M.! m > w'
                         then (M.insert m w' d,
                               H.insert (w',m) q)
                         else (d,q)
          where w' = w + weight (n',m)
      -- mark visited
      visited' = S.insert n' visited
  d = M.insert n 0
    $ M.fromList [(m, read "Infinity") | m <- M.keys adj]
  v = S.empty
  q = H.insert (0,n) H.empty

-- Search.test distanceWeight "./transform-bug.trunk-branch-self-loop.txt"
testWeighting w f = print . map w . makeGraph . phantomize =<< file f

-- find distances from first node in adj
-- testDijkstra distanceWeight "./sample1.txt"
testDijkstra w f = print . (\adj -> dijkstra w adj (M.keys adj !! 0)) . makeAdj . makeGraph . phantomize =<< file f
