{-# LANGUAGE TypeSynonymInstances #-}
module Graph where

import Data.List (groupBy, sort)
import Data.Function (on)

import qualified Data.Map as M
import Parse

type Segment = (PN, PN)
data Dir = F | B deriving (Eq,Show) -- direction
type Node = (Segment, Dir)
type Edge = (Node, Node)

-- map segment numbers (PS) to their node pair
makeSegmentMap :: [PNode] -> M.Map PS Segment
makeSegmentMap ps = m where
  -- learn some nodes (PN) connected to a segment (PS) from a PNode
  extract :: PNode -> [(PS,PN)]
  extract (PEndPoint n s)     = [(s,n)]
  extract (PSwitch n t b1 b2) = [(t,n), (b1,n), (b2,n)]

  -- group the extracted segment info by segment:
  --   [[(1,a1),(1,b1)], [(2,a2),(2,b2)], ...]
  groups :: [[(PS,PN)]]
  groups = groupBy ((==) `on` fst)
                   (sort (concatMap extract ps))

  -- convert to association list format
  regroup :: [(PS,PN)] -> (PS,Segment)
  regroup [(s1,n1),(s2,n2)] | s1 == s2 = (s1,(n1,n2)) -- s1 == s2 is a sanity check

  m = M.fromList (map regroup groups)

class Dual t where
  bar :: t -> t

instance Dual Dir where
  bar F = B
  bar B = F

instance Dual Segment where
  bar (a,b) = (b,a)

instance Dual Node where
  bar (a,b) = (bar a, bar b)

instance Dual Edge where
  bar (a,b) = (bar b, bar a)

-- return the other end of the segment
other :: Segment -> PN -> PN
other (a,b) n = a+b - n -- if a == n then b else a

-- compute the derived graph from the PNode info
makeGraph :: [PNode] -> [Edge]
makeGraph ps = reversals ++ transitions where
  m = makeSegmentMap ps
  reversals   = [((s',d), bar (s',d))
                | s  <- M.elems m,
                  s' <- [s, bar s],
                  d  <- [F,B]]
  transitions = [e'
                | PSwitch n t b1 b2 <- ps,
                  b <- [b1,b2],
                  d <- [F,B],
                  e <- [(((o t n,     n), d),
                         ((n,     o b n), d))],
                  e' <- [e, bar e]]
  o s n = other (m M.! s) n

main = do print . makeGraph =<< file "sample1.txt"
