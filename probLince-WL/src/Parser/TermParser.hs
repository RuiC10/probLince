module Parser.TermParser where

import Parser.DTermParser
import Text.Parsec

data Term = DetTerm DTerm 
          | COIN
          | RAND
          | NORMAL Term Term
          | NegateT Term
          | AddT Term Term 
          | SubT Term Term 
          | MultT Term Term 
          | DivT Term Term deriving Show

parseTerm :: Parsec String st Term
parseTerm = chainl1 parseAritTerm op
    where
        op = try (spaces >> string "+" >> spaces >> return AddT) 
          <|> try (spaces >> string "-" >> spaces >> return SubT)

parseAritTerm :: Parsec String st Term
parseAritTerm = chainl1 parseAritFactor op 
    where
        op = try (spaces >> string "*" >> spaces >> return MultT) 
          <|> try (spaces >> string "/" >> spaces >> return DivT)

parseAritFactor :: Parsec String st Term
parseAritFactor = try (NegateT <$> (string "-" >> parseAritFactor))
               <|> parseAritFactorAux

parseAritFactorAux :: Parsec String st Term
parseAritFactorAux = try (between (char '(') (char ')') parseTerm)
                  <|> try parseCoin 
                  <|> try parseRand
                  <|> try parseNormal
                  <|> (DetTerm <$> parseAritDFactor)

parseCoin :: Parsec String st Term
parseCoin = string "coin()" *> return COIN

parseRand :: Parsec String st Term
parseRand = string "rand()" *> return RAND

parseNormal :: Parsec String st Term
parseNormal = string "normal" *> between (char '(') (char ')') (NORMAL <$> parseTerm <*> (string "," *> parseTerm))
