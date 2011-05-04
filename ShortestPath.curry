module ShortestPath where

import FiniteMap
import Graph
import SetFunctions
import List(last)

member (e:_) = e
member (_:l) = member l

path :: WeightedGraph -> Node -> Node -> [(Int, Node)] -- Non-deterministically
path g n tn = (0, n) : (reverse $ path' g n tn [])
path' g n tn acc | lookupFM g n =:= Just edges 
                 & member edges =:= e
                 & notElem e acc =:= True = (path' g (snd e) tn (e : acc)) 
                 --? ((snd e) =:= tn &> e : acc)
     where edges, e free
path' g n tn acc | n =:= tn = acc
     --where edges, e free
           --allDifferent l = all (\x-> not $ any (\y-> x == y) l) l-}
path'set = set3 path

shortestPaths g n tn = minValue cmp paths
         where paths = path'set g n tn
               cmp a b = (totalLen a) < (totalLen b)
               totalLen l = foldl (+) 0 (map fst l)

node wg i = fst $ (fmToList wg) !! i

wg0 = weightGraph switchBackWeight $ toGraph p3
