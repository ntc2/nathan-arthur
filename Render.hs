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
  d  = dijkstra w n adj
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

-- create graphviz edge labels
segmentLabeling :: EdgeWeighting -> Node -> Adj -> [(PS,String)]
segmentLabeling w n adj = labeling where
  d = M.toList $ dijkstra w n adj
  sid = fst . fst -- segment id
  groups = groupBy ((==) `on` sid . fst) d
  label pairs@([(((i,_),_),_),_,_,_]) = (i, pairs)
  labeling = [(i,(pf "label=\"%s\"" $ show l) :: String)
             | g <- groups, l@(i,_) <- [label g]]

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
