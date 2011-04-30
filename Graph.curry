module Graph where

import Parse
import Array
import FiniteMap
import Unsafe
import List
import Combinatorial

data Dir = F | B
opposite F = B
opposite B = F

-- A Node in the transformed graph. (Point A, Point B, Going from A to B?, Facing direction of motion?)
type Node = ((Int, Int), Dir)

-- A graph as an adjacency list
type Graph = FM Node [Node]

toGraph :: [PNode] -> Graph
toGraph points = nodes
  where segments = toSegments points (emptyDefaultArray (const []) :: Array [(Bool,Int)])
        nodes = switchsToNodes points (segmentsToNodes segments 1 (emptyFM (<)))
        --nodes = segmentsToNodes segments 1 (emptyFM (<))

        -- si = segment index, pi = point index
        other :: Int -> Int -> Int 
        other si pi | (segments ! si) =:= [(unknown, pi), (unknown, oi)] = oi where oi free
        other si pi | (segments ! si) =:= [(unknown, oi), (unknown, pi)] = oi where oi free

        switchsToNodes tmp@((PSwitch pi ti b1i b2i) : pnts) ns = trace ("Switch " ++ (show (tmp, ns)) ++ "\n") $
                                     switchsToNodes pnts (addListToFM_C (++) ns $ concatMap (\d-> [
                                                         (((oti, pi), d), [((pi, ob1i), d), ((pi, ob2i), d)]),
                                                         (((ob1i, pi), d), [((pi, oti), d)]),
                                                         (((ob2i, pi), d), [((pi, oti), d)])
                                                         ]) [F, B])
                                where oti = other ti pi
                                      ob1i = other b1i pi
                                      ob2i = other b2i pi
        switchsToNodes ((PEndPoint _ _) : pnts) ns = switchsToNodes pnts ns
        switchsToNodes [] pnts = pnts

        segmentsToNodes segs i ns = trace ("segs2nodes " ++ (show (i, ns)) ++ "\n") $
                                      segmentsToNodes segs (i+1) (addListToFM_C (++) ns $ concatMap (\d -> [
                                             (((p1, p2), d), [((p2, p1), opposite d)]), 
                                             (((p2, p1), d), [((p1, p2), opposite d)])
                                             ]) [F, B])
                             where [(t1, p1), (t2, p2)] = segs ! i
        segmentsToNodes segs i ns | segs ! i =:= [] = ns

-- This needs to be changed to detect segments that start at a trunk and go to a branch of the same switch. It should add a psudo node on the loop.
toSegments ((PEndPoint ni ei) : ns) segs = 
                     toSegments ns (applyAt segs ei ((False,ni):)) -- Add End Point Edge
toSegments ((PSwitch ni ti e1i e2i) : ns) segs = 
                     toSegments ns (applyAt (applyAt (applyAt segs 
                                         ti ((True,ni):)) -- Add Trunk Edge
                                         e1i ((False,ni):)) -- Add Branch Segments
                                         e2i ((False,ni):))
toSegments [] segs = segs -- if all (2== . length) segs then segs else error $ "There is a segment without 2 nodes

p0 = [(PEndPoint 1 1),(PSwitch 2 1 2 3),(PEndPoint 3 2),(PEndPoint 4 3)]
p1 = [(PEndPoint 1 1),(PSwitch 2 1 2 12),(PEndPoint 3 4),(PSwitch 4 5 3 4),(PSwitch 5 3 2 7),(PSwitch 6 12 7 8),(PSwitch 7 6 5 9),(PEndPoint 8 6),(PSwitch 9 8 9 10),(PSwitch 10 13 10 11),(PEndPoint 11 11),(PEndPoint 12 13)]
--unsafePerformIO $ parseFile "sample1.txt"
p2 = [(PSwitch 1 15 1 15),(PSwitch 2 1 2 13),(PSwitch 3 2 3 8),(PSwitch 4 3 4 5),(PSwitch 5 6 5 4),(PSwitch 6 6 7 7),(PSwitch 7 10 8 9),(PEndPoint 8 14),(PSwitch 9 14 9 11),(PSwitch 10 12 11 16),(PSwitch 11 13 10 12),(PEndPoint 12 16)]
--unsafePerformIO $ parseFile "sample2.txt"
p3 = [PEndPoint 1 1, PEndPoint 4 3, PEndPoint 6 5,
      PSwitch 2 1 2 6, PSwitch 3 3 2 4, PSwitch 5 5 4 6]

p4 = [(PSwitch 1 1 2 1),(PEndPoint 2 2)]

p4_2 = [(PSwitch 1 1 2 3),(PSwitch 2 3 4 5),(PSwitch 3 1 4 5),(PEndPoint 4 2)]

p5 = [(PSwitch 1 1 2 2),(PEndPoint 2 1)]
