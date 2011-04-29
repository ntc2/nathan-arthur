module Graph where

import Data.List (groupBy, sort)
import Data.Function (on)

import qualified Data.Map as M
import Parse

type Segment = (PN, PN)
data Dir = F | B -- direction
type Node = (Segment, Dir)
type Edge = (Node, Node)

-- return the other end of the segment
other :: Segment -> PN -> PN
other (a,b) n = a+b - n -- if a == n then b else a

-- map segment numbers (PS) to their node pair
makeSegmentMap :: [PNode] -> M.Map PS Segment
makeSegmentMap ps = m where
  -- learn some nodes (PN) connected to a segment (PS) from a PNode
  extract :: PNode -> [(PS,PN)]
  extract (PEndPoint n s)     = [(s,n)]
  extract (PSwitch n t b1 b2) = [(t,n), (b1,n), (b2,n)]

  -- group the extracted segment info by segment
  groups :: [[(PS,PN)]]
  groups = groupBy ((==) `on` fst)
                   (sort (concatMap extract ps))

  -- convert to association list format
  regroup :: [(PS,PN)] -> (PS,Segment)
  regroup [(s1,n1),(s2,n2)] | s1 == s2 = (s1,(n1,n2)) -- s1 == s2 is a sanity check

  m = M.fromList (map regroup groups)

makeGraph = makeSegmentMap

main = do print . makeGraph =<< file "sample1.txt"
