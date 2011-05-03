{-# LANGUAGE TypeSynonymInstances #-}
module Graph where

import System.Environment (getArgs)

import Data.List (groupBy, sort)
import Data.Function (on)

import qualified Data.Map as M
import Parse

type Segment = (PN,PN) -- (from, to)
type NamedSegment = (PS, Segment) -- (id, (from,to))
data Dir = F | B deriving (Eq,Ord,Show) -- train direction
type Node = (NamedSegment, Dir)
type Edge = (Node, Node)

-- | eliminate self segments via phantom nodes and segments
phantomize :: [PNode] -> [PNode]
phantomize = concatMap f where
  -- this is easier to understand if you draw a picture ...
  f (PSwitch n t b1 b2) | t  == b1 = [ PEndPoint (-n) (-t)
                                     , PEndPoint (-n)   t
                                     , PSwitch n t (-t) b2]
                        | t  == b2 = [ PEndPoint (-n) (-t)
                                     , PEndPoint (-n)   t
                                     , PSwitch n t b1 (-t)]
                        | b1 == b2 = [ PEndPoint (-n) (-b1)
                                     , PEndPoint (-n)   b1
                                     , PSwitch n t b1 (-b1)]
  f p = [p]

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

instance Dual NamedSegment where
  bar (id,s) = (id, bar s)

instance Dual Node where
  bar (ns,d) = (bar ns, bar d)

instance Dual Edge where
  bar (a,b) = (bar b, bar a)

-- return the other end of the segment
other :: Segment -> PN -> PN
other (a,b) n = a+b - n -- if a == n then b else a

-- compute the derived graph from the PNode info
makeGraph :: [PNode] -> [Edge]
makeGraph ps = reversals ++ transitions where
  m = makeSegmentMap ps
  reversals   = [(n, bar n)
                | s  <- M.assocs m
                , s' <- [s, bar s]
                , d  <- [F,B]
                , n  <- [(s',d)]
                ]
  transitions = [e'
                | PSwitch n t b1 b2 <- ps
                , b <- [b1,b2]
                , d <- [F,B]
                , e <- [(((t, (o t n,      n)), d),
                         ((b, (n,      o b n)), d))]
                , e' <- [e, bar e]
                ]
  o s n = other (m M.! s) n

type Adj = M.Map Node [Node]
makeAdj :: [Edge] -> Adj
makeAdj es = m where
  groups :: [[Edge]]
  groups = groupBy ((==) `on` fst)
                   (sort es)
  regroup :: [Edge] -> (Node,[Node])
  regroup ((k,v):xs) = (k, v:map snd xs)
  m = M.fromList (map regroup groups)

test f = do print . makeGraph . phantomize =<< file f
testAdj f = do print . makeAdj . makeGraph . phantomize =<< file f
main = do test . (!! 0) =<< getArgs
