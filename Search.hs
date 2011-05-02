module Search where

import Graph
import Parse

-- these would be completely symmetric without the phantoms ...
distanceWeight, reversalWeight :: Edge -> Int
distanceWeight (((s,_),d),((s',_),d')) =
  if d /= d' ||
     -- don't count phantom transitions
     s < 0 || s' < 0
  then 0 else 1
reversalWeight ((_,    d),(_,     d')) =
  if d == d'
  then 0 else 1

-- Search.test distanceWeight "./transform-bug.trunk-branch-self-loop.txt"
test w f = print . map w . makeGraph . phantomize =<< file f
