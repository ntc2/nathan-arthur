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
  where segments = toSegments points
        nodes = switchsToNodes points (segmentsToNodes (eltsFM segments) (emptyFM (<)))
        --nodes = segmentsToNodes segments 1 (emptyFM (<))

        -- si = segment index, pi = point index
        other :: Int -> Int -> Int 
        other si pi | (lookupFM segments si) =:= Just [(unknown, pi), (unknown, oi)] = oi where oi free
        other si pi | (lookupFM segments si) =:= Just [(unknown, oi), (unknown, pi)] = oi where oi free

        switchsToNodes tmp@((PSwitch pi ti b1i b2i) : pnts) ns = 
                                     {-trace ("Switch " ++ (show (tmp, ns)) ++ "\n") $-}
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

        segmentsToNodes ([(t1, p1), (t2, p2)] : segs) ns = 
                                      {-trace ("segs2nodes " ++ (show ns) ++ "\n") $-}
                                      segmentsToNodes segs (addListToFM_C (++) ns $ concatMap (\d -> [
                                             (((p1, p2), d), [((p2, p1), opposite d)]), 
                                             (((p2, p1), d), [((p1, p2), opposite d)])
                                             ]) [F, B])
        segmentsToNodes [] ns = ns

-- This needs to be changed to detect segments that start at a trunk and go to a branch of the same switch. It should add a psudo node on the loop.
toSegments pns = toSegments' pns (emptyFM (<))
toSegments' ((PEndPoint ni ei) : ns) segs = 
                     toSegments' ns (addListToFM_C (++) segs [(ei, [(False,ni)])]) -- Add End Point Edge
toSegments' ((PSwitch ni ti e1i e2i) : ns) segs = 
                     toSegments' ns (addListToFM_C (++) segs 
                                         [(ti, [(True,ni)]),  -- Add Trunk Edge
                                         (e1i, [(False,ni)]), -- Add Branch Segments
                                         (e2i, [(False,ni)])])
toSegments' [] segs = if all ((2==) . length) (eltsFM segs) then segs else error $ "There is a segment without 2 points"

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

p6 = [PEndPoint 1 101,PSwitch 2 102 101 103,PSwitch 3 102 104 103,PEndPoint 4 104]

dat = [(PSwitch 102 122 121 119),(PSwitch 103 121 120 118),(PSwitch 106 116 117 111),(PSwitch 107 115 116 119),(PSwitch 108 114 115 118),(PSwitch 109 113 114 110),(PSwitch 110 112 113 109),(PSwitch 119 108 104 105),(PSwitch 114 103 104 105),(PSwitch 115 103 102 110),(PSwitch 116 102 109 101),(PSwitch 202 222 221 219),(PSwitch 203 508 220 218),(PSwitch 206 216 217 211),(PSwitch 207 215 216 219),(PSwitch 208 214 215 218),(PSwitch 209 213 214 210),(PSwitch 210 212 213 209),(PSwitch 219 208 204 205),(PSwitch 214 203 204 205),(PSwitch 215 203 202 210),(PSwitch 216 202 209 201),(PSwitch 302 322 321 319),(PSwitch 303 321 320 318),(PSwitch 306 316 317 311),(PSwitch 307 315 316 319),(PSwitch 308 314 315 318),(PSwitch 309 313 314 310),(PSwitch 310 312 313 309),(PSwitch 319 308 304 305),(PSwitch 314 303 304 305),(PSwitch 315 303 302 310),(PSwitch 316 302 309 301),(PSwitch 402 422 421 419),(PSwitch 403 421 420 418),(PSwitch 406 416 417 411),(PSwitch 407 415 416 419),(PSwitch 408 414 415 418),(PSwitch 409 413 414 410),(PSwitch 410 412 413 409),(PSwitch 419 408 404 405),(PSwitch 414 403 404 405),(PSwitch 415 403 402 410),(PSwitch 416 402 409 401),(PSwitch 502 522 521 519),(PSwitch 503 521 520 518),(PSwitch 506 516 517 511),(PSwitch 507 515 516 519),(PSwitch 508 514 515 518),(PSwitch 509 513 514 510),(PSwitch 510 512 513 509),(PSwitch 519 221 504 505),(PSwitch 514 503 504 505),(PSwitch 515 503 502 510),(PSwitch 516 502 509 501),(PSwitch 104 120 1 2),(PSwitch 111 112 3 4),(PEndPoint 117 101),(PEndPoint 217 201),(PSwitch 204 292 2 3),(PSwitch 211 212 1 4),(PSwitch 304 320 71 72),(PSwitch 311 312 73 74),(PEndPoint 317 301),(PSwitch 417 491 401 490),(PSwitch 404 420 72 73),(PSwitch 411 412 71 74),(PSwitch 201 222 11 12),(PSwitch 205 217 13 14),(PSwitch 212 211 15 16),(PSwitch 218 291 17 18),(PSwitch 301 322 11 13),(PSwitch 305 317 12 14),(PSwitch 312 311 15 17),(PSwitch 318 308 16 18),(PSwitch 401 422 21 22),(PSwitch 405 417 23 24),(PSwitch 412 411 25 26),(PSwitch 418 408 27 28),(PSwitch 501 522 21 23),(PSwitch 505 517 22 24),(PSwitch 512 511 25 27),(PSwitch 518 508 26 28),(PSwitch 101 122 31 33),(PSwitch 105 117 32 34),(PSwitch 112 111 35 35),(PSwitch 118 36 36 108),(PSwitch 504 520 31 32),(PSwitch 511 512 33 34),(PSwitch 517 591 501 590),(PEndPoint 601 490),(PEndPoint 602 590),(PSwitch 603 491 492 493),(PSwitch 604 591 592 493),(PSwitch 605 593 492 592),(PEndPoint 606 593),(PSwitch 611 293 208 220),(PSwitch 612 293 291 292)]