module Parse where

import Prelude hiding (lines)
import Text.Parsec -- ParserCombinators.Parsec -- hiding ((<|>))
-- import Text.Parsec.Token -- semi, colon, lexeme, natural ...
import Text.ParserCombinators.Parsec (parseFromFile)
import Control.Applicative hiding ((<|>), many)

-- For each contructor the first int is the node number and
-- the other ints are edge numbers with the first being the trunk
type PN = Int -- node type in parse input
type PS = Int -- segment type in parse input
data PNode = PEndPoint PN PS
           | PSwitch PN PS PS PS
             deriving Show

-- ??? how to do lexeme version tim described (Text.Parsec.Token) ?
--     
--     many (lexeme line)
--
--     natural
--
--     natural <* semi
--     natural <* colon
--     natural <* dot -- ???

--num = natural -- read <$> many1 digit
num = read <$> many1 digit
num_ c = num <* char c
pEndPoint = PEndPoint <$> num_ ':'
                      <*> num_ '.'
pSwitch = PSwitch <$> num_ ':' -- ':' 
                  <*> num_ ';' 
                  <*> num_ ',' 
                  <*> num
pNode = try pSwitch <|> pEndPoint

-- Parse each line and return a list.
lines =  (pNode `sepEndBy` spaces) <* eof

-- Parse the text in a file and return a parsed list in the IO monad.
file f = do p <- parseFromFile lines f
            case p of 
              Left e -> error $ show e -- ??? what's the right way to fail on parse error
              Right p -> return p

main = do
       d <- file "sample1.txt"
       print d
       e <- file "sample2.txt"
       print e
