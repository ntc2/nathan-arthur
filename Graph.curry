module Graph where

import Parse
import Array
import FiniteMap
import Unsafe
import List

-- A Node in the transformed graph. (Point A, Point B, Going from A to B?, Facing direction of motion?)
type Node = (Int, Int, Bool, Bool)

-- A graph as an adjacency list
type Graph = FM Node [Node]

--toGraph :: [PNode] -> Graph
toGraph points = nodes
  where segments = toSegments points (emptyDefaultArray (const []) :: Array [(Bool,Int)])
        nodes = toNodes segments (length points) (emptyFM (<))

        toNodes segs i ns | i > 0 = toNodes segs (i-1) (addListToFM_C (++) ns [
                                             ((p1, p2, True, True),[(p1, p2, False, False)]), 
                                             ((p1, p2, False, False),[(p1, p2, True, True)])
                                             ])
                             where [(t1, p1), (t2, p2)] = segs ! i
        toNodes _ 0 ns = ns

        toSegments ((PEndPoint ni ei) : ns) segs = 
                             toSegments ns (applyAt segs ei ((False,ni):)) -- Add End Point Edge
        toSegments ((PSwitch ni ti e1i e2i) : ns) segs = 
                             toSegments ns (applyAt (applyAt (applyAt segs 
                                                 ti ((True,ni):)) -- Add Trunk Edge
                                                 e1i ((False,ni):)) -- Add Branch Segments
                                                 e2i ((False,ni):))
        toSegments [] segs = segs -- if all (2== . length) segs then segs else error $ "There is a segment without 2 nodes
           

p1 = unsafePerformIO $ parseFile "sample1.txt"
p2 = unsafePerformIO $ parseFile "sample2.txt"
