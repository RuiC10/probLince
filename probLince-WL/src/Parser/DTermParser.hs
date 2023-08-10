module Parser.DTermParser where

import Text.Parsec
import Parser.LanguageDef
import qualified Text.Parsec.Token as L

data DTerm = Const Double 
           | Var String 
           | Negate DTerm
           | Add DTerm DTerm 
           | Sub DTerm DTerm 
           | Mult DTerm DTerm 
           | Div DTerm DTerm deriving Show

parseDTerm :: Parsec String st DTerm
parseDTerm = chainl1 parseAritDTerm op
    where
        op = try (spaces >> string "+" >> spaces >> return Add) <|> try (spaces >> string "-" >> spaces >> return Sub)

parseAritDTerm :: Parsec String st DTerm
parseAritDTerm = chainl1 parseAritDFactor op 
    where
        op = try (spaces >> string "*" >> spaces >> return Mult) <|> try (spaces >> string "/" >> spaces >> return Div)

parseAritDFactor :: Parsec String st DTerm
parseAritDFactor = try (Negate <$> (string "-" >> parseAritDFactor)) <|> parseAritDFactorAux

parseAritDFactorAux :: Parsec String st DTerm
parseAritDFactorAux = try (between (char '(') (char ')') parseDTerm) <|> try parseConst <|> parseVar 

parseConst :: Parsec String st DTerm
parseConst = L.naturalOrFloat hybridLangLexer >>= return . Const . either (fromIntegral) id 

parseVar :: Parsec String st DTerm
parseVar = Var <$> L.identifier hybridLangLexer
