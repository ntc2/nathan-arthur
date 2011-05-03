{-# LANGUAGE NoMonomorphismRestriction #-}
module Render where

-- hi tech ascii FFI ...
import Text.Printf
import Control.Arrow ((***),(&&&))
import qualified Data.Map as M

import Graph
import Parse
import Search

p = putStrLn
pf = printf

-- convert to arthur types
type ArthurNode = (Segment, PS, Dir)
node :: Node -> ArthurNode
node ((id,s),d) = (s,id,d)
weight :: Double -> Int
weight x = if x == read "Infinity"
                  then -1 else round x

type WeightedGraph = M.Map Node [(Int, ArthurNode)]
makeWeightedGraph :: EdgeWeighting -> Node -> Adj -> WeightedGraph
makeWeightedGraph w n adj = wg where
  d  = dijkstra w n adj
  wg = M.map (map ((weight . (d M.!)) &&& node)) adj

foo :: EdgeWeighting -> Node -> String -> IO String
foo w n f = do return
          . show
          . M.toList
          . makeWeightedGraph w n
          . makeAdj
          . makeGraph
          . phantomize
          =<< file f

main w n f = do
  p "import ReadShowTerm"
  p "import FiniteMap"
  p ""
  p "import Graph"
  p "import Rendering"
  p ""
  pf "main = renderDot \"tmp\" \"dot\" (graphToDot (listToFM (<) (readTerm \"%s\")))" =<< foo w n f
