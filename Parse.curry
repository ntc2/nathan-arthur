module Parse where

import ReadNumeric
import SetFunctions
import List

-- For each contructor the first int is the node number and
-- the other ints are edge numbers with the first being the trunk
data PNode = PEndPoint Int Int
           | PSwitch Int Int Int Int

parse :: String -> [PNode]
-- Parse each line and return a list. I don't think this can return
-- multiple different parses for the same input but I'm not 100% sure.
parse s = map parseLine $ lines s

-- Parse an int or fail.
read s | readInt s =:= Just (i, unknown) = i where i free

parseLine :: String -> PNode
-- Parse a line into a PNode or fail.
parseLine (sn++":"++se++".") 
          | n =:= read sn & e =:= read se = PEndPoint n e
              where n, e free
parseLine (sn++":"++st++";"++sb1++","++sb2) 
          | n =:= read sn & t =:= read st 
            & b1 =:= read sb1 & b2 =:= read sb2 = PSwitch n t b1 b2
              where n, t, b1, b2 free

-- Parse the text in a file and return a parsed list in the IO monad.
parseFile fn = do
               s <- readFile fn

               -- This forces s to be evaluated which it needs to be 
               -- or the results of the following are incorrect. Comment
               -- it out and you will get "No Solutions". This is a 
               -- PAKCS bug I am almost sure. Probably related to the 
               -- interaction of lazy IO with non-determinism.
               (last s) `seq` done 

               getSomeValue $ parse (filter (/='\r') s)

main = do
       d <- parseFile "input/sample1.txt"
       print d
       e <- parseFile "input/sample2.txt"
       print e

