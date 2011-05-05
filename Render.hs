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
import Control.Applicative
import System.FilePath.Posix

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
  ds = M.toList d
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

nodesForSegment sid sm = [((sid,(a,b)),d) | let s = sm M.! sid, (a,b) <- [s, bar s], d <- [F,B]]

p = putStrLn
pf = printf

comment description s = do
  pf "-- %s: %s\n" description s
  hPutStrLn stderr (description ++ ": " ++ s)

makeCurry' :: String -> EdgeWeighting -> Node -> PS -> M.Map PS Segment -> FilePath -> IO ()
makeCurry' out w ns se sm f = do
  adj <- pNodesToAdj <$> file f
  let (d, pp) = dijkstra w ns adj
  let nodes = nodesForSegment se sm
  let ne = minimumBy (compare `on` (\n -> (d M.! n, length $ extractPath pp n))) nodes
  let sid = fst . fst
  let nodes = extractPath pp ne
  comment "params" out
  comment "path long" $ show nodes
  let sids = (map sid $ nodes) ++ [se] -- hack: extractPath is buggy ...
  comment "path" $ show sids
  comment "length path" $ show $ length sids
  comment "weight path" $ show $ d M.! ne
  p "import Maybe"
  p ""
  p "import Graph"
  p "import Rendering"
  p "import Parse"
  p ""
  sl <- (segmentLabeling w ns . pNodesToAdj) <$> file f
  pf "f k = fromJust $ lookup k %s\n" $ show sl
  pf "path = %s\n" $ show sids
  pf "main = do pnodes <- parseFile \"%s\"\n" f
  pf "          renderDot \"%s\" \"dot\"" out
  pf "            (pnodesToDot' f path pnodes)\n"

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

-- usage: runhaskell Render.hs [d | r] START [y | n] END GRAPH > tmp.curry && pakcs -r tmp.curry
--
-- where START and END are segment ids, and [y | n] is flip
main = do
  [name,ws,sss,flips,ses,f] <- getArgs
  sm <- makeSegmentMap <$> file f
  let flip = if flips == "y" then bar else id
  let w = if ws == "d" then distanceWeight
          else if ws == "r" then reversalWeight
               else error $ "unknown weight function: " ++ ws
      ss = read sss
      ns = ((ss, flip $ sm M.! ss),F) :: Node
      se = read ses
  makeCurry' name w ns se sm f
  hPutStrLn stderr $ "search started from: " ++ show ns
