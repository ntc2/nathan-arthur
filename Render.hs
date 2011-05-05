{-# LANGUAGE NoMonomorphismRestriction #-}
module Render where

-- hi tech ascii FFI ...
import Text.Printf
import Control.Arrow ((&&&))
import qualified Data.Map as M
import System.Environment (getArgs)
import System.IO
import Data.List
import Data.Function

import Debug.Trace

import Graph
import Parse
import Search

-- convert to arthur types
type ArthurNode = (Segment, PS, Dir)
node :: Node -> ArthurNode
node ((id,s),d) = (s,id,d)
weight :: Double -> Int
weight x = if x == read "Infinity"
           then -1 else round x

type WeightedGraph = M.Map ArthurNode [(Int, ArthurNode)]
makeWeightedGraph :: EdgeWeighting -> Node -> Adj -> WeightedGraph
makeWeightedGraph w n adj = wg where
  d  = fst $ dijkstra w n adj
  wg = M.mapKeys node
     $ M.map (map ((weight . (d M.!)) &&& node)) adj

convert :: EdgeWeighting -> Node -> FilePath -> IO String
convert w n f = return
          . show
          . M.toList
          . makeWeightedGraph w n
          . makeAdj
          . makeGraph
          . phantomize
          =<< file f

pNodesToAdj :: [PNode] -> Adj
pNodesToAdj = makeAdj . makeGraph . phantomize

-- This is the original print all rendering
allInformation = id
-- This rendered the cost of the shortest path for all directions of travel and facings
minOfAll (_,ps) = truncate $ minimum (map snd ps)
-- This rendered the cost of the shortest path for either directions of travel 
-- where the train faces from s to d
minOfFacing (s,d) (i,ps) = truncate $ minimum (map snd ps')
    where ps' = filter (\(((_,(a,b)),facing),_)-> (facing == F && (a,b) == (s,d)) 
                                                  || (facing == B && (a,b) == (d,s)) ) ps

swap (a, b) = (b, a)

-- create graphviz edge labels
segmentLabeling :: EdgeWeighting -> Node -> Adj -> [(PS,String)]
segmentLabeling w n adj = labeling where
  (d, pp) = dijkstra w n adj
  ds = extractPath pp ((16,(212,318)),F) `traceShow` M.toList d
  sid = fst . fst . fst -- segment id
  groups = groupBy ((==) `on` sid) ds
  label pairs@([e,_,_,_]) = (sid e, pairs)
  labeling = [(i,(pf "label=\"%d %s\"" i $ lf l) :: String)
             | g <- groups, let l@(i,ps) = label g]
  -- Compute the label require a specific facing if you return to the same segment 
  -- but allowing any facing if you are going to a different segment
  lf l@(i,_) | i == (fst $ fst $ n) = let cost = minOfFacing (swap $ snd $ fst $ n) l in
                                      -- Ugly hack with a constant below but it will be big enough for all our graphs.
                                      "rev " ++ if cost < 1000000 then show cost 
                                                else "Impossible"
             | otherwise = "any " ++ (show $ minOfAll l)

p = putStrLn
pf = printf

makeCurry' :: EdgeWeighting -> Node -> FilePath -> IO ()
makeCurry' w n f = do
  p "import Maybe"
  p ""
  p "import Graph"
  p "import Rendering"
  p "import Parse"
  p ""
  sl <- return . segmentLabeling w n . pNodesToAdj =<< file f
  pf "f k = fromJust $ lookup k %s\n" $ show sl
  pf "main = do pnodes <- parseFile \"%s\"\n" f
  p  "          renderDot \"tmp\" \"dot\""
  pf "            (pnodesToDot' f pnodes)\n"

makeCurry :: EdgeWeighting -> Node -> FilePath -> IO ()
makeCurry w n f = do
  p "import FiniteMap"
  p ""
  p "import Graph"
  p "import Rendering"
  p ""
  p "main = renderDot \"tmp\" \"dot\""
  pf "         (weightedgraphToDot (listToFM (<) %s) False)\n"
     =<< convert w n f

main = do
  [ws,ns,f] <- getArgs
  sm <- return . makeSegmentMap =<< file f
  let w = if ws == "d" then distanceWeight
          else if ws == "r" then reversalWeight
               else error $ "unknown weight function: " ++ ws
      n = ((read ns, sm M.! read ns),F) :: Node
  makeCurry' w n f
  hPutStrLn stderr $ "search started from: " ++ show n
