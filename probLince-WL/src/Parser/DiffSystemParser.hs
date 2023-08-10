module Parser.DiffSystemParser where

import Text.Parsec
import Parser.LanguageDef
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.Parsec.Token as L

data DiffSystem = DiffSystem (Map String DiffTerm) deriving (Show)

data DiffTerm = DTVar String
              | DTConst Double
              | DTTime
              | MultDT DiffTerm DiffTerm
              | SumDT DiffTerm DiffTerm
              | DivDT DiffTerm DiffTerm
              | SubDT DiffTerm DiffTerm
              | NegateDT DiffTerm deriving (Show)

parseDiffSystem :: Parsec String st DiffSystem
parseDiffSystem = DiffSystem <$> chainl1 parseDiffEquation op
    where
        op = spaces >> string "," >> spaces >> return Map.union

parseDiffEquation :: Parsec String st (Map String DiffTerm)
parseDiffEquation = Map.singleton <$> (L.identifier hybridLangLexer <* string "'" <* spaces <* string "=" <* spaces) <*> (chainl1 parseDiffTerm op)
    where
        op = try (spaces >> string "+" >> spaces >> return SumDT) <|> try (spaces >> string "-" >> spaces >> return SubDT)

parseDiffTerm :: Parsec String st DiffTerm
parseDiffTerm = chainl1 parseDiffFactor op
    where
        op = try (spaces >> string "*" >> spaces >> return MultDT) <|> try (spaces >> string "/" >> spaces >> return DivDT)

parseDiffFactor :: Parsec String st DiffTerm
parseDiffFactor =  try (NegateDT <$> (string "-" >> parseDiffFactor)) 
               <|> try (between (char '(') (char ')') parseDiffTerm)
               <|> try (DTConst <$> parseDiffConst)
               <|> try (string "_time_" *> spaces *> return DTTime)
               <|> (DTVar <$> L.identifier hybridLangLexer)

parseDiffConst :: Parsec String st Double
parseDiffConst = try (string "-" *> L.naturalOrFloat hybridLangLexer >>= return . either (negate . fromIntegral) negate)
              <|> (L.naturalOrFloat hybridLangLexer >>= return . either fromIntegral id)
