module Athame.Read () where

import Athame.Types

import qualified Text.Parsec as P
import Text.Parsec (Parsec)
import Control.Applicative (pure, (<$>), (<*>), (<*), (*>))

type Parser = Parsec String ()

parseQuarter :: Parser QuarterTurn
parseQuarter = do c <- P.oneOf "FBUDLRMESxyzfbudlr"
                  return $ parse c
    where parse 'F' = F
          parse 'B' = B
          parse 'U' = U
          parse 'D' = D
          parse 'L' = L
          parse 'R' = R
          parse 'M' = M
          parse 'E' = E
          parse 'S' = S
          parse 'x' = X
          parse 'y' = Y
          parse 'z' = Z
          parse 'f' = Fw
          parse 'b' = Bw
          parse 'u' = Uw
          parse 'd' = Dw
          parse 'l' = Lw
          parse 'r' = Rw

parseTurn :: Parser Turn
parseTurn = do q <- parseQuarter
               c <- P.option Quarter parse2
               i <- P.option id parseI
               return (i (c q))
    where parse2 = P.char '2'  *> pure Half
          parseI = P.char '\'' *> pure Inverse

parseMove :: Parser Move
parseMove = P.choice [
                P.try parseElementary,
                P.try parseGroup,
                P.try parseRepeat,
                P.try parseSimultaneous,
                P.try parseTrigger,
                P.try parseConjugate,
                P.try parseCommutator,
                P.try parseInfix,
                P.try parseNamed
            ]
    where parseElementary = Elementary <$> parseTurn
          parseGroup = inParens "()" $ Group <$> parseMoves
          parseRepeat = Repeat <$> inParens "()" parseMoves <*> parseInt
          parseTrigger = inParens "<>" $ Trigger <$> parseMoves
          parseSimultaneous = surroundedPair Simultaneous "()" '/' parseTurn
          surroundedPair f parens sep p = inParens parens $ do
                                            a <- p
                                            P.spaces *> P.char sep <* P.spaces
                                            b <- p
                                            return $ f a b
          parseConjugate = surroundedPair Conjugate "[]" ':' parseMoves
          parseCommutator = surroundedPair Commutator "[]" ',' parseMoves
          parseInfix = inParens "[]" $ mkInfix <$> parseMove <*> stars <*> parseMove
          mkInfix a i b = Infix a b i
          stars = length <$> P.many1 (P.char '*')
          parseNamed = inParens "{}" $ Named <$> P.many1 (P.noneOf "}")

parseMoves :: Parser [Move]
parseMoves = parseMove `P.sepBy1` P.spaces

parseAlgorithm :: Parser Algorithm
parseAlgorithm = Algorithm <$> (parseName <* P.char ':') <*> (P.spaces *> parseMoves)
    where parseName = P.many1 $ P.noneOf ":"

instance Read QuarterTurn where
    readsPrec = readsPrecImpl parseQuarter
    readList = readListImpl parseQuarter P.spaces P.spaces P.spaces

instance Read Turn where
    readsPrec = readsPrecImpl parseTurn
    readList = readListImpl parseTurn P.spaces P.spaces P.spaces

instance Read Move where
    readsPrec = readsPrecImpl parseMove
    readList = readListImpl parseMove P.spaces P.spaces P.spaces

instance Read Algorithm where
    readsPrec = readsPrecImpl parseAlgorithm
    readList = readListImpl parseAlgorithm P.spaces P.spaces P.newline


-- Util stuff

inParens :: String -> Parser a -> Parser a
inParens [o,c] p = P.between (P.char o) (P.char c) p

parseInt :: Parser Int
parseInt = read <$> P.many1 P.digit

runParser :: Parser a -> String -> [(a, String)]
runParser p s = case P.runParser (p <^(,)^> P.lookAhead (P.many P.anyChar)) () "" s of
                    Left _ -> []
                    Right a -> [a]

readsPrecImpl :: Parser a -> Int -> ReadS a
readsPrecImpl p _ = runParser p

readListImpl :: Parser a -> Parser o -> Parser c -> Parser s -> ReadS [a]
readListImpl p o c sep = runParser $ o *> (p `P.sepBy` sep) <* c

-- Infix applicative
(<^) :: Parser a -> (a -> b) -> Parser b
(<^) = flip (<$>)
(^>) :: Parser (a -> b) -> Parser a -> Parser b
(^>) = (<*>)

