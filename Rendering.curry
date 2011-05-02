module Rendering where

import Graph
import Parse

import FiniteMap
import Array
import List
import IO
import System

nodeToIdent :: Node -> String
nodeToIdent ((a, b), e, d) = "n_"++(nshow a)++"_"++(nshow b)++"_"++(nshow e)++"_"++(show d)
nshow n | n < 0 = "n" ++ show (-n)
        | otherwise = show n 

graphToDot :: Graph -> Bool -> String
graphToDot g grouping = "digraph {\n" ++ (if grouping then groups l else "") ++ (concatMap h l) ++ "}\n"
      where l = fmToList g
            h (n, es) = concatMap (\e -> ni ++ " -> " ++ nodeToIdent e ++ ";\n") es
                  where ni = nodeToIdent n
            
            groups ns | getNodesForEdge ns =:= (these, rest) = 
                            "subgraph cluster_" ++ show a ++ "_" ++ show b ++ " {"++  
                              concatMap ((++";\n") . nodeToIdent) (map fst these) ++"}\n"++groups rest
                                          where these, rest free
                                                (((a,b), _, _),_) = head these
            groups [] = ""

{-getNodesForEdge these rest 
     | length r =:= (length these) + (length rest) & splitSet r =:= (these, rest) {-& length these =:= 4-} = r
     --& these =:= permute [((a,b),F),((b,a),F),((a,b),B),((b,a),B)] = r
                                    where r free -}

getNodesForEdge (n@(((a,b),_,_),_):ns) = (these, rest)
        where these = n : filter matches ns
              rest = filter (not . matches) ns
              matches (((oa, ob),_,_),_) = (a, b) == (oa, ob) || (a, b) == (ob, oa)

pnodesToDot pns = "graph {\n" ++ (h (eltsFM segs)) ++ "}\n"
            where segs = toSegments pns
                  h ([(t1, n1), (t2, n2)] : ss) = 
                      nshow n1 ++ " -- " ++ nshow n2 ++ "[" ++ attrs t2 t1 ++ "];\n" ++ h ss
                  h [] = ""
                  attrs h t = "arrowhead=" ++ arrowType h ++ ",arrowtail=" ++ arrowType t
                  arrowType True = "normal"
                  arrowType False = "inv"


renderDot :: String -> String -> String -> IO ()
renderDot baseFn renderer dot = do
          --let dot = toDot g grouping
          let dotFn = (baseFn ++ ".dot")
          let pngFn = (baseFn ++ ".png")
          f <- openFile dotFn WriteMode
          hPutStrLn f dot
          hClose f
          _ <- system (renderer ++ " -Tpng " ++ dotFn ++ " > " ++ pngFn)
          _ <- system ("eog " ++ pngFn)   
          done
