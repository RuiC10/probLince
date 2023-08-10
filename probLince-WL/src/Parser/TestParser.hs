module Parser.TestParser where

import Text.Parsec
import Parser.DTermParser

data Test = BTerm Bool 
          | Or Test Test 
          | And Test Test 
          | Not Test 
          | Gt DTerm DTerm 
          | Lt DTerm DTerm 
          | Eq DTerm DTerm deriving Show

parseTests :: Parsec String st Test
parseTests = chainl1 parseTestTerm op 
    where   
        op = try (spaces >> string "&&" >> spaces >> return And) <|> try (spaces >> string "||" >> spaces >> return Or)

parseTestTerm :: Parsec String st Test
parseTestTerm = Not <$> (char '!' >> parseTestFactor) <|> parseTestFactor

parseTestFactor :: Parsec String st Test
parseTestFactor = try (between (char '(') (char ')') parseTests) <|> try parseDTermComp <|> parseBool

parseBool :: Parsec String st Test
parseBool = (string "true" <|> string "false") >>= (\x -> if x == "true" then return (BTerm True) else return (BTerm False))

parseDTermComp :: Parsec String st Test
parseDTermComp = try gtComp <|> try ltComp <|> eqComp
    where
        gtComp = Gt <$> (parseDTerm <* spaces <* (string ">") <* spaces) <*> parseDTerm
        ltComp = Lt <$> (parseDTerm <* spaces <* (string "<") <* spaces) <*> parseDTerm
        eqComp = Eq <$> (parseDTerm <* spaces <* (string "==") <* spaces) <*> parseDTerm

